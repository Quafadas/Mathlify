package mathlify

import MathExpr.*
import scala.math
import scala.annotation.targetName

object Evaluator:

  // ── Free variable analysis ────────────────────────────────────────────────

  def freeVars[A](expr: MathExpr[A]): Set[String] = expr match
    case Number(_)                                        => Set.empty
    case Constant(_)                                      => Set.empty
    case Symbol(name) if symbolicConstants.contains(name) => Set.empty
    case Symbol(name)                                     => Set(name)
    case Add(l, r)                                        => freeVars(l) ++ freeVars(r)
    case Sub(l, r)                                        => freeVars(l) ++ freeVars(r)
    case Mul(l, r)                                        => freeVars(l) ++ freeVars(r)
    case Div(l, r)                                        => freeVars(l) ++ freeVars(r)
    case Pow(b, e)                                        => freeVars(b) ++ freeVars(e)
    case Neg(e)                                           => freeVars(e)
    case FunctionCall(_, args)                            => args.flatMap(freeVars).toSet
    case Fraction(n, d)                                   => freeVars(n) ++ freeVars(d)
    case Root(deg, rad)                                   => deg.map(freeVars).getOrElse(Set.empty) ++ freeVars(rad)
    case Sum(idx, lo, hi, body)                           => freeVars(idx) ++ freeVars(lo) ++ freeVars(hi) ++ freeVars(body)
    case Integral(v, lo, hi, body)                        => freeVars(v) ++ freeVars(lo) ++ freeVars(hi) ++ freeVars(body)
    case Group(e)                                         => freeVars(e)
    case MathVector(elems)                                => elems.flatMap(freeVars).toSet
    case Matrix(elems, _, _, _, _, _)                     => elems.flatMap(freeVars).toSet
    case Subscript(Symbol(base), Number(n))               =>
      // Subscript variables like x_1 are named by their Double index.
      (n: Any) match
        case d: Double => Set(s"${base}_${d.round.toInt}")
        case _         => freeVars(Symbol(base))
    case Subscript(b, s)   => freeVars(b) ++ freeVars(s)
    case Superscript(b, s) => freeVars(b) ++ freeVars(s)
    case Operator(_)       => Set.empty
    case ExprSeq(exprs)    =>
      def goSeq(items: List[MathExpr[A]]): Set[String] = items match
        case Nil                                                   => Set.empty
        case Symbol(_) :: (bg @ BracketGroup("(", ")", _)) :: rest =>
          // Symbol followed by ( ) is a function application: the function name
          // is not a free variable, but the arguments are.
          freeVars(bg) ++ goSeq(rest)
        case head :: rest => freeVars(head) ++ goSeq(rest)
      goSeq(exprs)
    case Over(b, t)            => freeVars(b) ++ freeVars(t)
    case Under(b, bot)         => freeVars(b) ++ freeVars(bot)
    case SubSup(b, s, sup)     => freeVars(b) ++ freeVars(s) ++ freeVars(sup)
    case Style(_, c)           => freeVars(c)
    case TextNode(_)           => Set.empty
    case BracketGroup(_, _, c) => freeVars(c)
    case Enclose(_, c)         => freeVars(c)
    case Color(_, c)           => freeVars(c)

  def isClosed[A](expr: MathExpr[A]): Boolean =
    freeVars(expr).isEmpty

  def unboundVars[A](expr: MathExpr[A], env: Map[String, A] = Map.empty): Set[String] =
    freeVars(expr) -- env.keySet

  def isEvaluable[A](expr: MathExpr[A], env: Map[String, A]): Boolean =
    freeVars(expr).subsetOf(env.keySet)

  // ── Constant folding (Double-only) ────────────────────────────────────────

  def foldConstants(expr: MathExpr[Double]): MathExpr[Double] = expr match
    case Add(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b))             => Number(a + b)
        case (Add(fl2, Number(c1)), Number(c2)) => foldConstants(Add(fl2, Number(c1 + c2)))
        case (fl, fr)                           => Add(fl, fr)
    case Sub(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b))             => Number(a - b)
        case (Add(fl2, Number(c1)), Number(c2)) => foldConstants(Add(fl2, Number(c1 - c2)))
        case (fl, fr)                           => Sub(fl, fr)
    case Mul(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b))                             => Number(a * b)
        case (fl, fr) if fl == Number(0.0) || fr == Number(0.0) => Number(0.0)
        case (Number(1.0), fr)                                  => fr
        case (fl, Number(1.0))                                  => fl
        case (Number(a), Add(fl2, fr2))                         => foldConstants(Add(Mul(Number(a), fl2), Mul(Number(a), fr2)))
        case (Number(a), Sub(fl2, fr2))                         => foldConstants(Sub(Mul(Number(a), fl2), Mul(Number(a), fr2)))
        case (Add(fl2, fr2), Number(b))                         => foldConstants(Add(Mul(Number(b), fl2), Mul(Number(b), fr2)))
        case (Sub(fl2, fr2), Number(b))                         => foldConstants(Sub(Mul(Number(b), fl2), Mul(Number(b), fr2)))
        case (Number(a), Group(Add(fl2, fr2)))                  => foldConstants(Add(Mul(Number(a), fl2), Mul(Number(a), fr2)))
        case (Number(a), Group(Sub(fl2, fr2)))                  => foldConstants(Sub(Mul(Number(a), fl2), Mul(Number(a), fr2)))
        case (Group(Add(fl2, fr2)), Number(b))                  => foldConstants(Add(Mul(Number(b), fl2), Mul(Number(b), fr2)))
        case (Group(Sub(fl2, fr2)), Number(b))                  => foldConstants(Sub(Mul(Number(b), fl2), Mul(Number(b), fr2)))
        case (fl, fr)                                           => Mul(fl, fr)
    case Div(l, r) =>
      (foldConstants(l), foldConstants(r)) match
        case (Number(a), Number(b)) if b != 0.0 => Number(a / b)
        case (fl, fr)                           => Div(fl, fr)
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
      end match
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
        case (fn, fd)                           => Fraction(fn, fd)
    case ExprSeq(exprs) =>
      val folded = exprs.map(foldConstants)
      simplifyExprSeq(folded) match
        case List(e) => e
        case other   => ExprSeq(other)
      end match
    case other => other

  // ── Symbolic constant substitution (Double-only) ──────────────────────────

  // Well-known constants that AsciiMath emits as Symbol nodes (e.g. "pi" -> Symbol("pi")).
  private val symbolicConstants: Map[String, Double] = Map(
    "π" -> math.Pi,
    "e" -> math.E
  )

  // Well-known constants emitted as Operator nodes (e.g. "infty" -> Operator("inf")).
  private val operatorConstants: Map[String, Double] = Map(
    "∞" -> Double.PositiveInfinity
  )

  /** Replace known symbolic/operator constant tokens with their numeric values. */
  private def substituteConstants(expr: MathExpr[Double]): MathExpr[Double] = expr match
    case Symbol(n) if symbolicConstants.contains(n)   => Number(symbolicConstants(n))
    case Operator(s) if operatorConstants.contains(s) => Number(operatorConstants(s))
    case Add(l, r)                                    => Add(substituteConstants(l), substituteConstants(r))
    case Sub(l, r)                                    => Sub(substituteConstants(l), substituteConstants(r))
    case Mul(l, r)                                    => Mul(substituteConstants(l), substituteConstants(r))
    case Div(l, r)                                    => Div(substituteConstants(l), substituteConstants(r))
    case Pow(b, e)                                    => Pow(substituteConstants(b), substituteConstants(e))
    case Neg(e)                                       => Neg(substituteConstants(e))
    case FunctionCall(n, args)                        => FunctionCall(n, args.map(substituteConstants))
    case Fraction(n, d)                               => Fraction(substituteConstants(n), substituteConstants(d))
    case Root(None, r)                                => Root(None, substituteConstants(r))
    case Root(Some(d), r)                             => Root(Some(substituteConstants(d)), substituteConstants(r))
    case Group(e)                                     => Group(substituteConstants(e))
    case ExprSeq(es)                                  => ExprSeq(es.map(substituteConstants))
    case BracketGroup(o, c, e)                        => BracketGroup(o, c, substituteConstants(e))
    case Superscript(b, s)                            => Superscript(substituteConstants(b), substituteConstants(s))
    case Subscript(b, s)                              => Subscript(substituteConstants(b), substituteConstants(s))
    case SubSup(b, s, sup)                            => SubSup(substituteConstants(b), substituteConstants(s), substituteConstants(sup))
    case other                                        => other

  /** Parse an AsciiMath string and evaluate it to a constant Double if possible. */
  def parseConstant(input: String): Option[Double] =
    AsciiMath.translate(input.trim) match
      case Left(_)     => None
      case Right(expr) =>
        eval(substituteConstants(expr)) match
          case Numeric(v) => Some(v)
          case _          => None

  // ── Full evaluation ───────────────────────────────────────────────────────

  /** Evaluate a Double expression with constant folding and symbolic constant substitution. */
  def eval(
      expr: MathExpr[Double],
      env: Map[String, Double] = Map.empty
  )(using alg: MathTrig[Double]): EvalResult[Double] =
    val folded = foldConstants(substituteConstants(expr))
    if !isEvaluable(folded, env) then EvalError(s"Unbound variables: ${(freeVars(folded) -- env.keySet).mkString(", ")}")
    else evalImpl(folded, env)
    end if
  end eval

  /** Evaluate a generic expression under the given algebra. */
  @targetName("evalGeneric")
  def eval[A](
      expr: MathExpr[A],
      env: Map[String, A]
  )(using alg: MathTrig[A]): EvalResult[A] =
    if !isEvaluable(expr, env) then EvalError(s"Unbound variables: ${(freeVars(expr) -- env.keySet).mkString(", ")}")
    else evalImpl(expr, env)
    end if
  end eval

  // ── Partial evaluation ────────────────────────────────────────────────────

  /** Partially evaluate a Double expression with constant folding and symbolic constant substitution. */
  def partialEval(
      expr: MathExpr[Double],
      env: Map[String, Double] = Map.empty
  )(using alg: MathTrig[Double]): EvalResult[Double] =
    val folded = foldConstants(substituteConstants(expr))
    if isEvaluable(folded, env) then evalImpl(folded, env)
    else PartiallyReduced(folded)
    end if
  end partialEval

  /** Partially evaluate a generic expression under the given algebra. */
  @targetName("partialEvalGeneric")
  def partialEval[A](
      expr: MathExpr[A],
      env: Map[String, A]
  )(using alg: MathTrig[A]): EvalResult[A] =
    if isEvaluable(expr, env) then evalImpl(expr, env)
    else PartiallyReduced(expr)
    end if
  end partialEval

  // ── Generic evaluator ─────────────────────────────────────────────────────

  private def evalImpl[A](
      expr: MathExpr[A],
      env: Map[String, A]
  )(using alg: MathTrig[A]): EvalResult[A] = expr match
    case Number(n)      => Numeric(n)
    case Constant(name) =>
      name match
        case "pi" | "π" => Numeric(alg.fromDouble(math.Pi))
        case "e"        => Numeric(alg.fromDouble(math.E))
        case other      => EvalError(s"Unknown constant: $other")
    case Symbol(name) =>
      env.get(name) match
        case Some(v) => Numeric(v)
        case None    => EvalError(s"Unbound variable: $name")
    case Add(l, r) =>
      (evalImpl(l, env), evalImpl(r, env)) match
        case (Numeric(a), Numeric(b)) => Numeric(alg.plus(a, b))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Add")
    case Sub(l, r) =>
      (evalImpl(l, env), evalImpl(r, env)) match
        case (Numeric(a), Numeric(b)) => Numeric(alg.minus(a, b))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Sub")
    case Mul(l, r) =>
      (evalImpl(l, env), evalImpl(r, env)) match
        case (Numeric(a), Numeric(b)) => Numeric(alg.times(a, b))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Mul")
    case Div(l, r) =>
      (evalImpl(l, env), evalImpl(r, env)) match
        case (Numeric(a), Numeric(b)) =>
          if b == alg.zero then EvalError("Division by zero")
          else Numeric(alg.div(a, b))
        case (e: EvalError, _) => e
        case (_, e: EvalError) => e
        case _                 => EvalError("Unexpected partial result in Div")
    case Pow(b, e) =>
      (evalImpl(b, env), evalImpl(e, env)) match
        case (Numeric(a), Numeric(n)) => Numeric(alg.pow(a, n))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Pow")
    case Superscript(b, e) =>
      (evalImpl(b, env), evalImpl(e, env)) match
        case (Numeric(a), Numeric(n)) => Numeric(alg.pow(a, n))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Superscript")
    case Neg(e) =>
      evalImpl(e, env) match
        case Numeric(a)   => Numeric(alg.negate(a))
        case e: EvalError => e
        case _            => EvalError("Unexpected partial result in Neg")
    case FunctionCall(name, args) =>
      args.map(a => evalImpl(a, env)) match
        case List(Numeric(a)) =>
          name match
            case "sin"  => Numeric(alg.sin(a))
            case "cos"  => Numeric(alg.cos(a))
            case "tan"  => Numeric(alg.tan(a))
            case "exp"  => Numeric(alg.exp(a))
            case "log"  => Numeric(alg.log(a))
            case "sqrt" => Numeric(alg.sqrt(a))
            case other  => EvalError(s"Unsupported function: $other")
        case List(e: EvalError) => e
        case _                  => EvalError(s"Unsupported function call: $name")
    case Root(None, rad) =>
      evalImpl(rad, env) match
        case Numeric(a)   => Numeric(alg.sqrt(a))
        case e: EvalError => e
        case _            => EvalError("Unexpected partial result in Root")
    case Root(Some(deg), rad) =>
      (evalImpl(deg, env), evalImpl(rad, env)) match
        case (Numeric(d), Numeric(a)) => Numeric(alg.pow(a, alg.div(alg.one, d)))
        case (e: EvalError, _)        => e
        case (_, e: EvalError)        => e
        case _                        => EvalError("Unexpected partial result in Root")
    case Subscript(Symbol(base), Number(n)) =>
      // Subscript variables like x_1 are looked up by their Double index key.
      (n: Any) match
        case d: Double =>
          val key = s"${base}_${d.round.toInt}"
          env.get(key) match
            case Some(v) => Numeric(v)
            case None    => EvalError(s"Unbound variable: $key")
          end match
        case _ => EvalError(s"Cannot evaluate subscript with non-numeric index")
    case Group(e)              => evalImpl(e, env)
    case BracketGroup(_, _, c) => evalImpl(c, env)
    case Fraction(n, d)        =>
      (evalImpl(n, env), evalImpl(d, env)) match
        case (Numeric(a), Numeric(b)) =>
          if b == alg.zero then EvalError("Division by zero")
          else Numeric(alg.div(a, b))
        case (e: EvalError, _) => e
        case (_, e: EvalError) => e
        case _                 => EvalError("Unexpected partial result in Fraction")
    case ExprSeq(exprs)   => evalInfixSeqImpl(exprs, env)
    case _: MathVector[?] => EvalError("Cannot evaluate MathVector node to a scalar")
    case _: Matrix[?]     => EvalError("Cannot evaluate Matrix node to a scalar")
    case other            => EvalError(s"Cannot evaluate: ${other.getClass.getSimpleName}")

  // ── Generic ExprSeq infix evaluator ──────────────────────────────────────

  private def evalInfixSeqImpl[A](
      exprs: List[MathExpr[A]],
      env: Map[String, A]
  )(using alg: MathTrig[A]): EvalResult[A] =

    def parseAdd(items: List[MathExpr[A]]): (EvalResult[A], List[MathExpr[A]]) =
      val (lv, rest) = parseMul(items)
      parseAddRest(lv, rest)
    end parseAdd

    def parseAddRest(left: EvalResult[A], items: List[MathExpr[A]]): (EvalResult[A], List[MathExpr[A]]) =
      items match
        case Operator("+") :: rest =>
          val (rv, remaining) = parseMul(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(alg.plus(a, b))
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot add")
          parseAddRest(combined, remaining)
        case Operator("-") :: rest =>
          val (rv, remaining) = parseMul(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(alg.minus(a, b))
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot subtract")
          parseAddRest(combined, remaining)
        case Operator("=") :: rest =>
          val (rv, remaining) = parseAdd(rest)
          (rv, remaining)
        case _ => (left, items)

    def parseMul(items: List[MathExpr[A]]): (EvalResult[A], List[MathExpr[A]]) =
      val (lv, rest) = parsePrimary(items)
      parseMulRest(lv, rest)
    end parseMul

    def parseMulRest(left: EvalResult[A], items: List[MathExpr[A]]): (EvalResult[A], List[MathExpr[A]]) =
      items match
        case Operator(op) :: rest if op == "⋅" || op == "×" || op == "*" =>
          val (rv, remaining) = parsePrimary(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(alg.times(a, b))
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot multiply")
          parseMulRest(combined, remaining)
        case Operator("/") :: rest =>
          val (rv, remaining) = parsePrimary(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) =>
              if b == alg.zero then EvalError("Division by zero")
              else Numeric(alg.div(a, b))
            case (e: EvalError, _) => e
            case (_, e: EvalError) => e
            case _                 => EvalError("Cannot divide")
          parseMulRest(combined, remaining)
        case (head :: _) if !head.isInstanceOf[Operator] =>
          val (rv, remaining) = parsePrimary(items)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(alg.times(a, b))
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot multiply (implicit)")
          parseMulRest(combined, remaining)
        case _ => (left, items)

    def parsePrimary(items: List[MathExpr[A]]): (EvalResult[A], List[MathExpr[A]]) =
      items match
        case Nil                   => (EvalError("Unexpected end of expression"), Nil)
        case Operator("-") :: rest =>
          val (v, remaining) = parsePrimary(rest)
          val negated = v match
            case Numeric(a)   => Numeric(alg.negate(a))
            case e: EvalError => e
            case _            => EvalError("Cannot negate")
          (negated, remaining)
        case expr :: rest => (evalImpl(expr, env), rest)

    val (result, remaining) = parseAdd(exprs)
    if remaining.nonEmpty then EvalError(s"Unexpected elements in ExprSeq: ${remaining.map(_.getClass.getSimpleName).mkString(", ")}")
    else result
    end if
  end evalInfixSeqImpl

  // ── ExprSeq constant simplification (Double-only) ────────────────────────

  private def simplifyExprSeq(exprs: List[MathExpr[Double]]): List[MathExpr[Double]] =
    var segmentsRev = List.empty[(Option[String], List[MathExpr[Double]])]
    var currentOp: Option[String] = None
    var currentTermsRev: List[MathExpr[Double]] = Nil

    def flush(): Unit =
      if currentTermsRev.nonEmpty then
        segmentsRev = (currentOp, currentTermsRev.reverse) :: segmentsRev
        currentTermsRev = Nil

    for e <- exprs do
      e match
        case Operator(s @ ("+" | "-")) => flush(); currentOp = Some(s)
        case other                     => currentTermsRev = other :: currentTermsRev
    end for
    flush()
    val segments = segmentsRev.reverse

    val evaluated: List[(Option[String], Either[Double, List[MathExpr[Double]]])] =
      segments.map { case (op, terms) =>
        if terms.exists(e => freeVars(e).nonEmpty) then (op, Right(terms))
        else
          evalInfixSeqImpl(terms, Map.empty) match
            case Numeric(v) => (op, Left(v))
            case _          => (op, Right(terms))
      }

    @annotation.tailrec
    def mergeConsts(
        items: List[(Option[String], Either[Double, List[MathExpr[Double]]])],
        acc: List[(Option[String], Either[Double, List[MathExpr[Double]]])]
    ): List[(Option[String], Either[Double, List[MathExpr[Double]]])] =
      items match
        case Nil                                        => acc.reverse
        case (op1, Left(v1)) :: (op2, Left(v2)) :: rest =>
          val s1 = if op1.contains("-") then -v1 else v1
          val s2 = if op2.contains("-") then -v2 else v2
          val total = s1 + s2
          val merged: (Option[String], Either[Double, List[MathExpr[Double]]]) = op1 match
            case None    => (None, Left(total))
            case Some(_) => if total >= 0 then (Some("+"), Left(total)) else (Some("-"), Left(-total))
          mergeConsts(merged :: rest, acc)
        case item :: rest => mergeConsts(rest, item :: acc)

    val merged = mergeConsts(evaluated, Nil)

    merged.flatMap {
      case (None, Left(v))          => List(Number(v))
      case (Some("+"), Left(v))     => List(Operator("+"), Number(v))
      case (Some("-"), Left(v))     => List(Operator("-"), Number(v))
      case (Some(op), Left(v))      => List(Operator(op), Number(v))
      case (None, Right(terms))     => terms
      case (Some(op), Right(terms)) => Operator(op) :: terms
    }
  end simplifyExprSeq

end Evaluator
