package mathlify

import MathExpr.*
import spire.math.Jet
import spire.math.JetDim
import spire.implicits.given

/** Forward-mode automatic differentiation using Spire's `Jet[Double]` dual numbers.
  *
  * A `Jet[Double]` pairs a function value (`real`) with its derivative(s) (`infinitesimal`), so that derivative information propagates automatically through arithmetic and
  * transcendental operations.
  *
  * We use `JetDim(1)` throughout: a single infinitesimal dimension, which suffices for computing one partial derivative per evaluation pass.
  */
object ForwardDiff:

  /** Dimension used for single-variable forward-mode AD. */
  private given dim: JetDim = JetDim(1)

  /** Bridge from Spire's algebra for `Jet[Double]` to mathlify's `MathTrig` type class. */
  given MathTrig[Jet[Double]] with
    val zero: Jet[Double] = Jet.zero[Double]
    val one: Jet[Double] = Jet.one[Double]
    def fromLong(n: Long): Jet[Double] = Jet(n.toDouble)
    def fromDouble(d: Double): Jet[Double] = Jet(d)

    def plus(a: Jet[Double], b: Jet[Double]): Jet[Double] = a + b
    def minus(a: Jet[Double], b: Jet[Double]): Jet[Double] = a - b
    def times(a: Jet[Double], b: Jet[Double]): Jet[Double] = a * b
    def negate(a: Jet[Double]): Jet[Double] = -a
    def div(a: Jet[Double], b: Jet[Double]): Jet[Double] = a / b

    def pow(a: Jet[Double], b: Jet[Double]): Jet[Double] =
      if b.infinitesimal(0) == 0.0 then
        // Constant exponent: use power rule directly for numerical stability
        val v = scala.math.pow(a.real, b.real)
        val d = b.real * scala.math.pow(a.real, b.real - 1.0) * a.infinitesimal(0)
        Jet(v, Array(d))
      else
        // General case: a^b = exp(b * ln(a))
        val v = scala.math.pow(a.real, b.real)
        val d = v * (b.infinitesimal(0) * scala.math.log(a.real) + b.real * a.infinitesimal(0) / a.real)
        Jet(v, Array(d))

    def sin(a: Jet[Double]): Jet[Double] = spire.math.sin(a)
    def cos(a: Jet[Double]): Jet[Double] = spire.math.cos(a)
    def tan(a: Jet[Double]): Jet[Double] = spire.math.tan(a)
    def exp(a: Jet[Double]): Jet[Double] = spire.math.exp(a)
    def log(a: Jet[Double]): Jet[Double] = spire.math.log(a)
    def sqrt(a: Jet[Double]): Jet[Double] = spire.math.sqrt(a)
  end given

  given MathShow[Jet[Double]] with
    def show(a: Jet[Double]): String =
      val vs =
        if a.real % 1 == 0 && !a.real.isInfinite then a.real.toLong.toString
        else a.real.toString
      val d = a.infinitesimal(0)
      val ds =
        if d % 1 == 0 && !d.isInfinite then d.toLong.toString
        else d.toString
      s"($vs, $ds)"
    end show
  end given

  /** Compute the value and partial derivative of an expression with respect to a given variable.
    *
    * @param expr
    *   the parsed expression
    * @param env
    *   variable bindings (name → value)
    * @param withRespectTo
    *   the variable to differentiate with respect to
    * @return
    *   an `EvalResult[Jet[Double]]` where `Numeric(jet)` gives both `jet.real` (function value) and `jet.infinitesimal(0)` (partial derivative)
    */
  def differentiate(
      expr: MathExpr[Double],
      env: Map[String, Double],
      withRespectTo: String
  ): EvalResult[Jet[Double]] =
    val prepared = Evaluator.foldConstants(Evaluator.substituteConstantsPublic(expr))
    val lifted = liftToJet(prepared)
    val jetEnv: Map[String, Jet[Double]] = env.map { case (name, value) =>
      name -> Jet(value, Array(if name == withRespectTo then 1.0 else 0.0))
    }
    Evaluator.eval[Jet[Double]](lifted, jetEnv)
  end differentiate

  /** Lift a `MathExpr[Double]` to `MathExpr[Jet[Double]]` by wrapping all `Number(d)` nodes. */
  private def liftToJet(expr: MathExpr[Double]): MathExpr[Jet[Double]] = expr match
    case Number(v)                      => Number(Jet(v, Array(0.0)))
    case Symbol(n)                      => Symbol(n)
    case Constant(n)                    => Constant(n)
    case Add(l, r)                      => Add(liftToJet(l), liftToJet(r))
    case Sub(l, r)                      => Sub(liftToJet(l), liftToJet(r))
    case Mul(l, r)                      => Mul(liftToJet(l), liftToJet(r))
    case Div(l, r)                      => Div(liftToJet(l), liftToJet(r))
    case Pow(b, e)                      => Pow(liftToJet(b), liftToJet(e))
    case Neg(e)                         => Neg(liftToJet(e))
    case FunctionCall(n, args)          => FunctionCall(n, args.map(liftToJet))
    case Fraction(n, d)                 => Fraction(liftToJet(n), liftToJet(d))
    case Root(deg, rad)                 => Root(deg.map(liftToJet), liftToJet(rad))
    case Group(e)                       => Group(liftToJet(e))
    case ExprSeq(es)                    => ExprSeq(es.map(liftToJet))
    case BracketGroup(o, c, e)          => BracketGroup(o, c, liftToJet(e))
    case Superscript(b, s)              => Superscript(liftToJet(b), liftToJet(s))
    case Subscript(b, s)                => Subscript(liftToJet(b), liftToJet(s))
    case Operator(s)                    => Operator(s)
    case TextNode(c)                    => TextNode(c)
    case SubSup(b, sub, sup)            => SubSup(liftToJet(b), liftToJet(sub), liftToJet(sup))
    case Over(b, t)                     => Over(liftToJet(b), liftToJet(t))
    case Under(b, bot)                  => Under(liftToJet(b), liftToJet(bot))
    case Style(v, c)                    => Style(v, liftToJet(c))
    case Enclose(n, c)                  => Enclose(n, liftToJet(c))
    case Color(col, c)                  => Color(col, liftToJet(c))
    case Sum(idx, lo, hi, body)         => Sum(liftToJet(idx), liftToJet(lo), liftToJet(hi), liftToJet(body))
    case Integral(v, lo, hi, body)      => Integral(liftToJet(v), liftToJet(lo), liftToJet(hi), liftToJet(body))
    case MathVector(elems)              => MathVector(elems.map(liftToJet))
    case Matrix(elems, r, c, rs, cs, o) => Matrix(elems.map(liftToJet), r, c, rs, cs, o)
end ForwardDiff
