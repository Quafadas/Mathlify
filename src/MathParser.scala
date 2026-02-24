package mathlify

import fastparse.*
import fastparse.SingleLineWhitespace.*
import MathExpr.*

object MathParser:

  private val knownConstants = Set("pi", "e", "inf")

  def number(using P[Any]): P[MathExpr] = {
    import fastparse.NoWhitespace.*
    P((CharsWhileIn("0-9") ~ ("." ~ CharsWhileIn("0-9")).?).!).map { s =>
      Number(s.toDouble)
    }
  }

  def identifier(using P[Any]): P[String] = {
    import fastparse.NoWhitespace.*
    P((CharIn("a-zA-Z") ~ CharsWhileIn("a-zA-Z0-9", 0)).!)
  }

  def braceContent(using P[Any]): P[MathExpr] = P("{" ~ expr ~ "}")

  def simpleAtom(using P[Any]): P[MathExpr] =
    P(number | identifier.map { name =>
      if knownConstants.contains(name) then Constant(name) else Symbol(name)
    })

  def braceOrAtom(using P[Any]): P[MathExpr] = P(braceContent | simpleAtom)

  def sumIndexBound(using P[Any]): P[(MathExpr, MathExpr)] =
    P("{" ~ expr ~ "=" ~ expr ~ "}")

  def args(using P[Any]): P[List[MathExpr]] =
    P(expr ~ ("," ~ expr).rep).map { case (h, t) => h :: t.toList }

  def sqrtExpr(using P[Any]): P[MathExpr] =
    P("sqrt" ~ "(" ~ expr ~ ")").map(arg => Root(None, arg))

  def sumExpr(using P[Any]): P[MathExpr] =
    P("sum_" ~ sumIndexBound ~ "^" ~ braceOrAtom ~ expr).map { case (idx, lo, hi, body) =>
      Sum(idx, lo, hi, body)
    }

  def integralExpr(using P[Any]): P[MathExpr] =
    P("int_" ~ braceOrAtom ~ "^" ~ braceOrAtom ~ expr).map { case (lo, hi, body) =>
      Integral(Symbol("x"), lo, hi, body)
    }

  def parenExpr(using P[Any]): P[MathExpr] =
    P("(" ~ expr ~ ")").map(Group(_))

  def funcCallOrSymbol(using P[Any]): P[MathExpr] =
    P(identifier ~ ("(" ~ args ~ ")").?).map {
      case (name, Some(argList)) => FunctionCall(name, argList)
      case (name, None) =>
        if knownConstants.contains(name) then Constant(name)
        else Symbol(name)
    }

  def atom(using P[Any]): P[MathExpr] =
    P(number | sqrtExpr | sumExpr | integralExpr | parenExpr | funcCallOrSymbol)

  def postfix(using P[Any]): P[MathExpr] =
    P(atom ~ ("_" ~ atom).?).map {
      case (base, Some(sub)) => Subscript(base, sub)
      case (base, None)      => base
    }

  def negExpr(using P[Any]): P[MathExpr] =
    P(("-" ~/ negExpr).map(Neg(_)) | postfix)

  def powExpr(using P[Any]): P[MathExpr] =
    P(negExpr ~ ("^" ~ negExpr).?).map {
      case (base, Some(exp)) => Pow(base, exp)
      case (base, None)      => base
    }

  def mulOp(using P[Any]): P[(String, MathExpr)] =
    P(
      ("*" | "/").! ~ powExpr |
        (&(CharIn("a-zA-Z(")) ~ powExpr).map("*" -> _)
    )

  def mulExpr(using P[Any]): P[MathExpr] =
    P(powExpr ~ mulOp.rep).map { case (head, ops) =>
      ops.foldLeft(head) {
        case (acc, ("*", rhs)) => Mul(acc, rhs)
        case (acc, ("/", rhs)) => Div(acc, rhs)
        case (acc, _)          => acc
      }
    }

  def addExpr(using P[Any]): P[MathExpr] =
    P(mulExpr ~ (("+"|"-").! ~ mulExpr).rep).map { case (head, ops) =>
      ops.foldLeft(head) {
        case (acc, ("+", rhs)) => Add(acc, rhs)
        case (acc, ("-", rhs)) => Sub(acc, rhs)
        case (acc, _)          => acc
      }
    }

  def expr(using P[Any]): P[MathExpr] = P(addExpr)

  def parse(input: String): Either[String, MathExpr] =
    fastparse.parse(input.trim, { implicit p: P[Any] => expr ~ End }) match
      case Parsed.Success(value, _) => Right(value)
      case f: Parsed.Failure        => Left(f.msg)
