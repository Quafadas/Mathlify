package mathlify

import MathExpr.*
import scala.math

object Evaluator:

  // ── Free variable analysis ────────────────────────────────────────────────

  def freeVars(expr: MathExpr): Set[String] = expr match
    case Number(_)                     => Set.empty
    case Constant(_)                   => Set.empty
    case Symbol(name)                  => Set(name)
    case Add(l, r)                     => freeVars(l) ++ freeVars(r)
    case Sub(l, r)                     => freeVars(l) ++ freeVars(r)
    case Mul(l, r)                     => freeVars(l) ++ freeVars(r)
    case Div(l, r)                     => freeVars(l) ++ freeVars(r)
    case Pow(b, e)                     => freeVars(b) ++ freeVars(e)
    case Neg(e)                        => freeVars(e)
    case FunctionCall(_, args)         => args.flatMap(freeVars).toSet
    case Fraction(n, d)                => freeVars(n) ++ freeVars(d)
    case Root(deg, rad)                => deg.map(freeVars).getOrElse(Set.empty) ++ freeVars(rad)
    case Sum(idx, lo, hi, body)        => freeVars(idx) ++ freeVars(lo) ++ freeVars(hi) ++ freeVars(body)
    case Integral(v, lo, hi, body)     => freeVars(v) ++ freeVars(lo) ++ freeVars(hi) ++ freeVars(body)
    case Group(e)                      => freeVars(e)
    case MathVector(elems)             => elems.flatMap(freeVars).toSet
    case Matrix(elems, _, _, _, _, _)  => elems.flatMap(freeVars).toSet
    case Subscript(b, s)               => freeVars(b) ++ freeVars(s)
    case Superscript(b, s)             => freeVars(b) ++ freeVars(s)
    case Operator(_)                   => Set.empty
    case ExprSeq(exprs)                => exprs.flatMap(freeVars).toSet
    case Over(b, t)                    => freeVars(b) ++ freeVars(t)
    case Under(b, bot)                 => freeVars(b) ++ freeVars(bot)
    case SubSup(b, s, sup)             => freeVars(b) ++ freeVars(s) ++ freeVars(sup)
    case Style(_, c)                   => freeVars(c)
    case TextNode(_)                   => Set.empty
    case BracketGroup(_, _, c)         => freeVars(c)
    case Enclose(_, c)                 => freeVars(c)
    case Color(_, c)                   => freeVars(c)

  def isClosed(expr: MathExpr): Boolean =
    freeVars(expr).isEmpty

  def isEvaluable(expr: MathExpr, env: Map[String, Double]): Boolean =
    freeVars(expr).subsetOf(env.keySet)

  // ── Constant folding ──────────────────────────────────────────────────────

  def foldConstants(expr: MathExpr): MathExpr = expr match
    case Add(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b)) => Number(a + b)
        case (fl, fr)               => Add(fl, fr)
    case Sub(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b)) => Number(a - b)
        case (fl, fr)               => Sub(fl, fr)
    case Mul(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b)) => Number(a * b)
        case (fl, fr) if fl == Number(0.0) || fr == Number(0.0) => Number(0.0)
        case (Number(1.0), fr)      => fr
        case (fl, Number(1.0))      => fl
        case (fl, fr)               => Mul(fl, fr)
    case Div(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b)) if b != 0.0 => Number(a / b)
        case (fl, fr)                            => Div(fl, fr)
    case Pow(b, e) =>
      (foldConstants(b), foldConstants(e)) match
        case (Number(a), Number(n)) => Number(math.pow(a, n))
        case (fb, fe)               => Pow(fb, fe)
    case Neg(e) =>
      foldConstants(e) match
        case Number(a) => Number(-a)
        case fe        => Neg(fe)
    case FunctionCall(name, args) =>
      val foldedArgs = args.map(foldConstants)
      foldedArgs match
        case List(Number(a)) =>
          name match
            case "sin" => Number(math.sin(a))
            case "cos" => Number(math.cos(a))
            case "exp" => Number(math.exp(a))
            case "log" => Number(math.log(a))
            case _     => FunctionCall(name, foldedArgs)
        case _ => FunctionCall(name, foldedArgs)
    case Root(None, rad) =>
      foldConstants(rad) match
        case Number(a) => Number(math.sqrt(a))
        case fr        => Root(None, fr)
    case Root(Some(deg), rad) =>
      (foldConstants(deg), foldConstants(rad)) match
        case (Number(d), Number(a)) => Number(math.pow(a, 1.0 / d))
        case (fd, fr)               => Root(Some(fd), fr)
    case Group(e) =>
      Group(foldConstants(e))
    case Fraction(n, d) =>
      (foldConstants(n), foldConstants(d)) match
        case (Number(a), Number(b)) if b != 0.0 => Number(a / b)
        case (fn, fd)                            => Fraction(fn, fd)
    case other => other

  // ── Full evaluation ───────────────────────────────────────────────────────

  def eval(expr: MathExpr, env: Map[String, Double] = Map.empty): EvalResult =
    val folded = foldConstants(expr)
    if !isEvaluable(folded, env) then
      EvalError(s"Unbound variables: ${(freeVars(folded) -- env.keySet).mkString(", ")}")
    else
      evaluateNumeric(folded, env)

  // ── Partial evaluation ────────────────────────────────────────────────────

  def partialEval(expr: MathExpr, env: Map[String, Double] = Map.empty): EvalResult =
    val folded = foldConstants(expr)
    if isEvaluable(folded, env) then evaluateNumeric(folded, env)
    else PartiallyReduced(folded)

  // ── Internal numeric evaluator ────────────────────────────────────────────

  private def evaluateNumeric(
      expr: MathExpr,
      env: Map[String, Double]
  ): EvalResult = expr match
    case Number(n) => Numeric(n)
    case Constant(name) =>
      name match
        case "pi" | "π" => Numeric(math.Pi)
        case "e"        => Numeric(math.E)
        case other      => EvalError(s"Unknown constant: $other")
    case Symbol(name) =>
      env.get(name) match
        case Some(v) => Numeric(v)
        case None    => EvalError(s"Unbound variable: $name")
    case Add(l, r) =>
      (evaluateNumeric(l, env), evaluateNumeric(r, env)) match
        case (Numeric(a), Numeric(b)) => Numeric(a + b)
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Add")
    case Sub(l, r) =>
      (evaluateNumeric(l, env), evaluateNumeric(r, env)) match
        case (Numeric(a), Numeric(b)) => Numeric(a - b)
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Sub")
    case Mul(l, r) =>
      (evaluateNumeric(l, env), evaluateNumeric(r, env)) match
        case (Numeric(a), Numeric(b)) => Numeric(a * b)
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Mul")
    case Div(l, r) =>
      (evaluateNumeric(l, env), evaluateNumeric(r, env)) match
        case (Numeric(a), Numeric(b)) =>
          if b == 0.0 then EvalError("Division by zero")
          else Numeric(a / b)
        case (e: EvalError, _) => e
        case (_, e: EvalError) => e
        case _                 => EvalError("Unexpected partial result in Div")
    case Pow(b, e) =>
      (evaluateNumeric(b, env), evaluateNumeric(e, env)) match
        case (Numeric(a), Numeric(n)) => Numeric(math.pow(a, n))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Pow")
    case Neg(e) =>
      evaluateNumeric(e, env) match
        case Numeric(a)  => Numeric(-a)
        case e: EvalError => e
        case _            => EvalError("Unexpected partial result in Neg")
    case FunctionCall(name, args) =>
      args.map(a => evaluateNumeric(a, env)) match
        case List(Numeric(a)) =>
          name match
            case "sin" => Numeric(math.sin(a))
            case "cos" => Numeric(math.cos(a))
            case "exp" => Numeric(math.exp(a))
            case "log" => Numeric(math.log(a))
            case other => EvalError(s"Unsupported function: $other")
        case List(e: EvalError) => e
        case _                  => EvalError(s"Unsupported function call: $name")
    case Root(None, rad) =>
      evaluateNumeric(rad, env) match
        case Numeric(a)   => Numeric(math.sqrt(a))
        case e: EvalError => e
        case _            => EvalError("Unexpected partial result in Root")
    case Root(Some(deg), rad) =>
      (evaluateNumeric(deg, env), evaluateNumeric(rad, env)) match
        case (Numeric(d), Numeric(a)) => Numeric(math.pow(a, 1.0 / d))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Root")
    case Group(e) => evaluateNumeric(e, env)
    case Fraction(n, d) =>
      (evaluateNumeric(n, env), evaluateNumeric(d, env)) match
        case (Numeric(a), Numeric(b)) =>
          if b == 0.0 then EvalError("Division by zero")
          else Numeric(a / b)
        case (e: EvalError, _) => e
        case (_, e: EvalError) => e
        case _                 => EvalError("Unexpected partial result in Fraction")
    case other => EvalError(s"Cannot evaluate: ${other.getClass.getSimpleName}")
