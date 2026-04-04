package mathlify

import MathExpr.*

/** A dual number for forward-mode automatic differentiation.
  *
  * A `Dual(value, deriv)` pairs a function value with its derivative, so that derivative information propagates
  * automatically through arithmetic and transcendental operations.
  */
final case class Dual(value: Double, deriv: Double)

object Dual:

  /** MathTrig instance for Dual numbers, implementing forward-mode AD rules. */
  given MathTrig[Dual] with
    val zero = Dual(0.0, 0.0)
    val one = Dual(1.0, 0.0)
    def fromLong(n: Long) = Dual(n.toDouble, 0.0)
    def fromDouble(d: Double) = Dual(d, 0.0)

    def plus(a: Dual, b: Dual) = Dual(a.value + b.value, a.deriv + b.deriv)
    def minus(a: Dual, b: Dual) = Dual(a.value - b.value, a.deriv - b.deriv)
    def negate(a: Dual) = Dual(-a.value, -a.deriv)

    // Product rule: (fg)' = f'g + fg'
    def times(a: Dual, b: Dual) =
      Dual(a.value * b.value, a.deriv * b.value + a.value * b.deriv)

    // Quotient rule: (f/g)' = (f'g - fg') / g²
    def div(a: Dual, b: Dual) =
      val v = a.value / b.value
      val d = (a.deriv * b.value - a.value * b.deriv) / (b.value * b.value)
      Dual(v, d)

    // Power rule: (f^g)' = f^g * (g' * ln(f) + g * f'/f)
    // Special-cased for constant exponent: (f^n)' = n * f^(n-1) * f'
    def pow(a: Dual, b: Dual) =
      if b.deriv == 0.0 then
        // Constant exponent: power rule
        val v = scala.math.pow(a.value, b.value)
        val d = b.value * scala.math.pow(a.value, b.value - 1.0) * a.deriv
        Dual(v, d)
      else
        // General case: f^g = exp(g * ln(f))
        val v = scala.math.pow(a.value, b.value)
        val d = v * (b.deriv * scala.math.log(a.value) + b.value * a.deriv / a.value)
        Dual(v, d)

    def sin(a: Dual) = Dual(scala.math.sin(a.value), scala.math.cos(a.value) * a.deriv)
    def cos(a: Dual) = Dual(scala.math.cos(a.value), -scala.math.sin(a.value) * a.deriv)
    def tan(a: Dual) =
      val c = scala.math.cos(a.value)
      Dual(scala.math.tan(a.value), a.deriv / (c * c))
    def exp(a: Dual) =
      val e = scala.math.exp(a.value)
      Dual(e, e * a.deriv)
    def log(a: Dual) = Dual(scala.math.log(a.value), a.deriv / a.value)
    def sqrt(a: Dual) =
      val s = scala.math.sqrt(a.value)
      Dual(s, a.deriv / (2.0 * s))
  end given

  given MathShow[Dual] with
    def show(a: Dual): String =
      val vs = if a.value % 1 == 0 && !a.value.isInfinite then a.value.toLong.toString else a.value.toString
      val ds = if a.deriv % 1 == 0 && !a.deriv.isInfinite then a.deriv.toLong.toString else a.deriv.toString
      s"($vs, $ds)"
  end given
end Dual

object ForwardDiff:

  /** Compute the value and partial derivative of an expression with respect to a given variable.
    *
    * @param expr
    *   the parsed expression
    * @param env
    *   variable bindings (name → value)
    * @param withRespectTo
    *   the variable to differentiate with respect to
    * @return
    *   an `EvalResult[Dual]` where `Numeric(Dual(f, f'))` gives both the function value and its derivative
    */
  def differentiate(
      expr: MathExpr[Double],
      env: Map[String, Double],
      withRespectTo: String
  ): EvalResult[Dual] =
    val prepared = Evaluator.foldConstants(Evaluator.substituteConstantsPublic(expr))
    val lifted = liftToDual(prepared)
    val dualEnv: Map[String, Dual] = env.map { case (name, value) =>
      name -> Dual(value, if name == withRespectTo then 1.0 else 0.0)
    }
    Evaluator.eval[Dual](lifted, dualEnv)
  end differentiate

  /** Lift a `MathExpr[Double]` to `MathExpr[Dual]` by wrapping all `Number(d)` nodes with `Dual(d, 0.0)`. */
  private def liftToDual(expr: MathExpr[Double]): MathExpr[Dual] = expr match
    case Number(v)                 => Number(Dual(v, 0.0))
    case Symbol(n)                 => Symbol(n)
    case Constant(n)               => Constant(n)
    case Add(l, r)                 => Add(liftToDual(l), liftToDual(r))
    case Sub(l, r)                 => Sub(liftToDual(l), liftToDual(r))
    case Mul(l, r)                 => Mul(liftToDual(l), liftToDual(r))
    case Div(l, r)                 => Div(liftToDual(l), liftToDual(r))
    case Pow(b, e)                 => Pow(liftToDual(b), liftToDual(e))
    case Neg(e)                    => Neg(liftToDual(e))
    case FunctionCall(n, args)     => FunctionCall(n, args.map(liftToDual))
    case Fraction(n, d)            => Fraction(liftToDual(n), liftToDual(d))
    case Root(deg, rad)            => Root(deg.map(liftToDual), liftToDual(rad))
    case Group(e)                  => Group(liftToDual(e))
    case ExprSeq(es)               => ExprSeq(es.map(liftToDual))
    case BracketGroup(o, c, e)     => BracketGroup(o, c, liftToDual(e))
    case Superscript(b, s)         => Superscript(liftToDual(b), liftToDual(s))
    case Subscript(b, s)           => Subscript(liftToDual(b), liftToDual(s))
    case Operator(s)               => Operator(s)
    case TextNode(c)               => TextNode(c)
    case SubSup(b, sub, sup)       => SubSup(liftToDual(b), liftToDual(sub), liftToDual(sup))
    case Over(b, t)                => Over(liftToDual(b), liftToDual(t))
    case Under(b, bot)             => Under(liftToDual(b), liftToDual(bot))
    case Style(v, c)               => Style(v, liftToDual(c))
    case Enclose(n, c)             => Enclose(n, liftToDual(c))
    case Color(col, c)             => Color(col, liftToDual(c))
    case Sum(idx, lo, hi, body)    => Sum(liftToDual(idx), liftToDual(lo), liftToDual(hi), liftToDual(body))
    case Integral(v, lo, hi, body) => Integral(liftToDual(v), liftToDual(lo), liftToDual(hi), liftToDual(body))
    case MathVector(elems)         => MathVector(elems.map(liftToDual))
    case Matrix(elems, r, c, rs, cs, o) => Matrix(elems.map(liftToDual), r, c, rs, cs, o)
end ForwardDiff
