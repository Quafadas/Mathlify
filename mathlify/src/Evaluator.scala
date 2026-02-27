package mathlify

import MathExpr.*
import scala.math

object Evaluator:

  // ── Free variable analysis ────────────────────────────────────────────────

  def freeVars(expr: MathExpr): Set[String] = expr match
    case Number(_)                    => Set.empty
    case Constant(_)                  => Set.empty
    case Symbol(name)                 => Set(name)
    case Add(l, r)                    => freeVars(l) ++ freeVars(r)
    case Sub(l, r)                    => freeVars(l) ++ freeVars(r)
    case Mul(l, r)                    => freeVars(l) ++ freeVars(r)
    case Div(l, r)                    => freeVars(l) ++ freeVars(r)
    case Pow(b, e)                    => freeVars(b) ++ freeVars(e)
    case Neg(e)                       => freeVars(e)
    case FunctionCall(_, args)        => args.flatMap(freeVars).toSet
    case Fraction(n, d)               => freeVars(n) ++ freeVars(d)
    case Root(deg, rad)               => deg.map(freeVars).getOrElse(Set.empty) ++ freeVars(rad)
    case Sum(idx, lo, hi, body)       => freeVars(idx) ++ freeVars(lo) ++ freeVars(hi) ++ freeVars(body)
    case Integral(v, lo, hi, body)    => freeVars(v) ++ freeVars(lo) ++ freeVars(hi) ++ freeVars(body)
    case Group(e)                     => freeVars(e)
    case MathVector(elems)            => elems.flatMap(freeVars).toSet
    case Matrix(elems, _, _, _, _, _) => elems.flatMap(freeVars).toSet
    case Subscript(b, s)              => freeVars(b) ++ freeVars(s)
    case Superscript(b, s)            => freeVars(b) ++ freeVars(s)
    case Operator(_)                  => Set.empty
    case ExprSeq(exprs)               => exprs.flatMap(freeVars).toSet
    case Over(b, t)                   => freeVars(b) ++ freeVars(t)
    case Under(b, bot)                => freeVars(b) ++ freeVars(bot)
    case SubSup(b, s, sup)            => freeVars(b) ++ freeVars(s) ++ freeVars(sup)
    case Style(_, c)                  => freeVars(c)
    case TextNode(_)                  => Set.empty
    case BracketGroup(_, _, c)        => freeVars(c)
    case Enclose(_, c)                => freeVars(c)
    case Color(_, c)                  => freeVars(c)

  def isClosed(expr: MathExpr): Boolean =
    freeVars(expr).isEmpty

  def unboundVars(expr: MathExpr, env: Map[String, Double] = Map.empty): Set[String] =
    freeVars(expr) -- env.keySet

  def isEvaluable(expr: MathExpr, env: Map[String, Double]): Boolean =
    freeVars(expr).subsetOf(env.keySet)

  // ── Constant folding ──────────────────────────────────────────────────────

  def foldConstants(expr: MathExpr): MathExpr = expr match
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

  // ── Symbolic constant substitution ───────────────────────────────────────

  // Well-known constants that AsciiMath emits as Symbol nodes (e.g. "pi" → Symbol("π")).
  private val symbolicConstants: Map[String, Double] = Map(
    "π" -> math.Pi,
    "e" -> math.E
  )

  // Well-known constants emitted as Operator nodes (e.g. "infty" → Operator("∞")).
  private val operatorConstants: Map[String, Double] = Map(
    "∞" -> Double.PositiveInfinity
  )

  /** Replace known symbolic/operator constant tokens with their numeric values. */
  private def substituteConstants(expr: MathExpr): MathExpr = expr match
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
    case other                                        => other

  /** Parse an AsciiMath string and evaluate it to a constant Double if possible.
    *
    * Recognises well-known mathematical constants (π, e, ∞) in addition to numeric literals and arithmetic, so expressions such as "pi", "2*pi", "e^2" and "sqrt(2)" are accepted.
    *
    * @return
    *   Some(value) when the expression is a closed constant expression, None when the expression contains free variables or cannot be evaluated numerically.
    */
  def parseConstant(input: String): Option[Double] =
    AsciiMath.translate(input.trim) match
      case Left(_)     => None
      case Right(expr) =>
        eval(substituteConstants(expr)) match
          case Numeric(v) => Some(v)
          case _          => None

  // ── Full evaluation ───────────────────────────────────────────────────────

  def eval(expr: MathExpr, env: Map[String, Double] = Map.empty): EvalResult =
    val folded = foldConstants(expr)
    if !isEvaluable(folded, env) then EvalError(s"Unbound variables: ${(freeVars(folded) -- env.keySet).mkString(", ")}")
    else evaluateNumeric(folded, env)
    end if
  end eval

  // ── Partial evaluation ────────────────────────────────────────────────────

  def partialEval(expr: MathExpr, env: Map[String, Double] = Map.empty): EvalResult =
    val folded = foldConstants(expr)
    if isEvaluable(folded, env) then evaluateNumeric(folded, env)
    else PartiallyReduced(folded)
    end if
  end partialEval

  // ── Internal numeric evaluator ────────────────────────────────────────────

  private def evaluateNumeric(
      expr: MathExpr,
      env: Map[String, Double]
  ): EvalResult = expr match
    case Number(n)      => Numeric(n)
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
        case Numeric(a)   => Numeric(-a)
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
    case Group(e)              => evaluateNumeric(e, env)
    case BracketGroup(_, _, c) => evaluateNumeric(c, env)
    case Fraction(n, d)        =>
      (evaluateNumeric(n, env), evaluateNumeric(d, env)) match
        case (Numeric(a), Numeric(b)) =>
          if b == 0.0 then EvalError("Division by zero")
          else Numeric(a / b)
        case (e: EvalError, _) => e
        case (_, e: EvalError) => e
        case _                 => EvalError("Unexpected partial result in Fraction")
    case ExprSeq(exprs) => evalInfixSeq(exprs, env)
    case other          => EvalError(s"Cannot evaluate: ${other.getClass.getSimpleName}")

  // ── ExprSeq infix evaluator with precedence ───────────────────────────────

  private def evalInfixSeq(exprs: List[MathExpr], env: Map[String, Double]): EvalResult =

    // Additive: term (('+' | '-') term)*
    def parseAdd(items: List[MathExpr]): (EvalResult, List[MathExpr]) =
      val (lv, rest) = parseMul(items)
      parseAddRest(lv, rest)
    end parseAdd

    def parseAddRest(left: EvalResult, items: List[MathExpr]): (EvalResult, List[MathExpr]) =
      items match
        case Operator("+") :: rest =>
          val (rv, remaining) = parseMul(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(a + b)
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot add")
          parseAddRest(combined, remaining)
        case Operator("-") :: rest =>
          val (rv, remaining) = parseMul(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(a - b)
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot subtract")
          parseAddRest(combined, remaining)
        case _ => (left, items)

    // Multiplicative: primary (('⋅' | '×' | '/') primary)*
    def parseMul(items: List[MathExpr]): (EvalResult, List[MathExpr]) =
      val (lv, rest) = parsePrimary(items)
      parseMulRest(lv, rest)
    end parseMul

    def parseMulRest(left: EvalResult, items: List[MathExpr]): (EvalResult, List[MathExpr]) =
      items match
        case Operator(op) :: rest if op == "⋅" || op == "×" || op == "*" =>
          val (rv, remaining) = parsePrimary(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) => Numeric(a * b)
            case (e: EvalError, _)        => e
            case (_, e: EvalError)        => e
            case _                        => EvalError("Cannot multiply")
          parseMulRest(combined, remaining)
        case Operator("/") :: rest =>
          val (rv, remaining) = parsePrimary(rest)
          val combined = (left, rv) match
            case (Numeric(a), Numeric(b)) =>
              if b == 0.0 then EvalError("Division by zero")
              else Numeric(a / b)
            case (e: EvalError, _) => e
            case (_, e: EvalError) => e
            case _                 => EvalError("Cannot divide")
          parseMulRest(combined, remaining)
        case _ => (left, items)

    // Primary: optional unary '-' then a single MathExpr
    def parsePrimary(items: List[MathExpr]): (EvalResult, List[MathExpr]) =
      items match
        case Nil                   => (EvalError("Unexpected end of expression"), Nil)
        case Operator("-") :: rest =>
          val (v, remaining) = parsePrimary(rest)
          val negated = v match
            case Numeric(a)   => Numeric(-a)
            case e: EvalError => e
            case _            => EvalError("Cannot negate")
          (negated, remaining)
        case expr :: rest => (evaluateNumeric(expr, env), rest)

    val (result, remaining) = parseAdd(exprs)
    if remaining.nonEmpty then EvalError(s"Unexpected elements in ExprSeq: ${remaining.map(_.getClass.getSimpleName).mkString(", ")}")
    else result
    end if
  end evalInfixSeq

  // ── ExprSeq constant simplification ──────────────────────────────────────

  private def simplifyExprSeq(exprs: List[MathExpr]): List[MathExpr] =
    // Split the flat infix sequence at top-level + and - operators into
    // additive segments: (leadingOp: Option[String], terms: List[MathExpr]).
    // leadingOp is None for the first segment, Some("+") / Some("-") for the rest.
    // Build lists by prepending and reversing to avoid O(n²) appends.
    var segmentsRev = List.empty[(Option[String], List[MathExpr])]
    var currentOp: Option[String] = None
    var currentTermsRev: List[MathExpr] = Nil

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

    // For each segment, attempt evaluation if it contains no free variables.
    val evaluated: List[(Option[String], Either[Double, List[MathExpr]])] =
      segments.map { case (op, terms) =>
        if terms.exists(e => freeVars(e).nonEmpty) then (op, Right(terms))
        else
          evalInfixSeq(terms, Map.empty) match
            case Numeric(v) => (op, Left(v))
            case _          => (op, Right(terms))
      }

    // Merge consecutive constant segments by summing their signed values.
    @annotation.tailrec
    def mergeConsts(
        items: List[(Option[String], Either[Double, List[MathExpr]])],
        acc: List[(Option[String], Either[Double, List[MathExpr]])]
    ): List[(Option[String], Either[Double, List[MathExpr]])] =
      items match
        case Nil                                        => acc.reverse
        case (op1, Left(v1)) :: (op2, Left(v2)) :: rest =>
          val s1 = if op1.contains("-") then -v1 else v1
          val s2 = if op2.contains("-") then -v2 else v2
          val total = s1 + s2
          val merged: (Option[String], Either[Double, List[MathExpr]]) = op1 match
            case None    => (None, Left(total))
            case Some(_) => if total >= 0 then (Some("+"), Left(total)) else (Some("-"), Left(-total))
          mergeConsts(merged :: rest, acc)
        case item :: rest => mergeConsts(rest, item :: acc)

    val merged = mergeConsts(evaluated, Nil)

    // Reassemble into a flat list of MathExpr tokens.
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
