package mathlify

import MathExpr.*
import spire.math.Jet
import spire.math.JetDim
import spire.implicits.given

/** Forward-mode automatic differentiation using Spire's `Jet[Double]`.
  *
  * Uses `JetDim(n)` where `n` is the number of free variables, assigning one infinitesimal dimension per variable. All partial derivatives are computed simultaneously in a single
  * evaluator pass — the i-th component of the result's infinitesimal array holds ∂f/∂xᵢ.
  */
object ForwardDiff:

  /** Result of computing all partial derivatives in a single pass. */
  case class DiffResult(value: Double, partials: Map[String, Double])

  /** Create a MathTrig[Jet[Double]] for a specific number of infinitesimal dimensions. */
  private def makeMathTrig(n: Int): MathTrig[Jet[Double]] =
    given JetDim = JetDim(n)
    new MathTrig[Jet[Double]]:
      val zero: Jet[Double] = Jet.zero[Double]
      val one: Jet[Double] = Jet.one[Double]
      def fromLong(l: Long): Jet[Double] = Jet(l.toDouble)
      def fromDouble(d: Double): Jet[Double] = Jet(d)

      def plus(a: Jet[Double], b: Jet[Double]): Jet[Double] = a + b
      def minus(a: Jet[Double], b: Jet[Double]): Jet[Double] = a - b
      def times(a: Jet[Double], b: Jet[Double]): Jet[Double] = a * b
      def negate(a: Jet[Double]): Jet[Double] = -a
      def div(a: Jet[Double], b: Jet[Double]): Jet[Double] = a / b

      // Multi-dimensional power rule: computes ∂(a^b)/∂xᵢ for each dimension i
      def pow(a: Jet[Double], b: Jet[Double]): Jet[Double] =
        val dim = a.infinitesimal.length
        val v = scala.math.pow(a.real, b.real)
        val isConstExp = b.infinitesimal.forall(_ == 0.0)
        val inf = Array.tabulate(dim) { i =>
          if isConstExp then b.real * scala.math.pow(a.real, b.real - 1.0) * a.infinitesimal(i)
          else v * (b.infinitesimal(i) * scala.math.log(a.real) + b.real * a.infinitesimal(i) / a.real)
        }
        Jet(v, inf)
      end pow

      def sin(a: Jet[Double]): Jet[Double] = spire.math.sin(a)
      def cos(a: Jet[Double]): Jet[Double] = spire.math.cos(a)
      def tan(a: Jet[Double]): Jet[Double] = spire.math.tan(a)
      def exp(a: Jet[Double]): Jet[Double] = spire.math.exp(a)
      def log(a: Jet[Double]): Jet[Double] = spire.math.log(a)
      def sqrt(a: Jet[Double]): Jet[Double] = spire.math.sqrt(a)
    end new
  end makeMathTrig

  given MathShow[Jet[Double]] with
    def show(a: Jet[Double]): String =
      def fmtD(d: Double) = if d % 1 == 0 && !d.isInfinite then d.toLong.toString else d.toString
      val vs = fmtD(a.real)
      if a.infinitesimal.length == 1 then s"($vs, ${fmtD(a.infinitesimal(0))})"
      else
        val ds = a.infinitesimal.map(fmtD).mkString("[", ", ", "]")
        s"($vs, $ds)"
      end if
    end show
  end given

  /** Compute the value and ALL partial derivatives in a single evaluator pass.
    *
    * Uses `JetDim(n)` where `n = env.size`, assigning one infinitesimal dimension per variable (sorted alphabetically). The result contains the function value and a map from each
    * variable name to its partial derivative.
    */
  def gradient(
      expr: MathExpr[Double],
      env: Map[String, Double]
  ): Either[String, DiffResult] =
    val prepared = Evaluator.foldConstants(Evaluator.substituteConstantsPublic(expr))
    val varNames = env.keys.toSeq.sorted
    val n = varNames.size
    if n == 0 then
      Evaluator.eval(prepared) match
        case Numeric(v)   => Right(DiffResult(v, Map.empty))
        case EvalError(m) => Left(m)
        case _            => Left("Expression could not be fully evaluated")
    else
      given alg: MathTrig[Jet[Double]] = makeMathTrig(n)
      val lifted = liftToJet(prepared, n)
      val jetEnv: Map[String, Jet[Double]] = varNames.zipWithIndex.map { case (name, idx) =>
        val inf = Array.fill(n)(0.0)
        inf(idx) = 1.0
        name -> Jet(env(name), inf)
      }.toMap
      Evaluator.eval[Jet[Double]](lifted, jetEnv) match
        case Numeric(j) =>
          val partials = varNames.zipWithIndex.map { case (name, idx) =>
            name -> j.infinitesimal(idx)
          }.toMap
          Right(DiffResult(j.real, partials))
        case EvalError(msg) => Left(msg)
        case _              => Left("Expression could not be fully evaluated")
      end match
    end if
  end gradient

  /** Compute the value and partial derivative w.r.t. one variable (convenience wrapper around `gradient`). */
  def differentiate(
      expr: MathExpr[Double],
      env: Map[String, Double],
      withRespectTo: String
  ): Either[String, (Double, Double)] =
    gradient(expr, env).map(dr => (dr.value, dr.partials.getOrElse(withRespectTo, 0.0)))

  /** Lift a `MathExpr[Double]` to `MathExpr[Jet[Double]]` with the given number of infinitesimal dimensions. */
  private def liftToJet(expr: MathExpr[Double], dim: Int): MathExpr[Jet[Double]] = expr match
    case Number(v)                      => Number(Jet(v, Array.fill(dim)(0.0)))
    case Symbol(n)                      => Symbol(n)
    case Constant(n)                    => Constant(n)
    case Add(l, r)                      => Add(liftToJet(l, dim), liftToJet(r, dim))
    case Sub(l, r)                      => Sub(liftToJet(l, dim), liftToJet(r, dim))
    case Mul(l, r)                      => Mul(liftToJet(l, dim), liftToJet(r, dim))
    case Div(l, r)                      => Div(liftToJet(l, dim), liftToJet(r, dim))
    case Pow(b, e)                      => Pow(liftToJet(b, dim), liftToJet(e, dim))
    case Neg(e)                         => Neg(liftToJet(e, dim))
    case FunctionCall(n, args)          => FunctionCall(n, args.map(liftToJet(_, dim)))
    case Fraction(n, d)                 => Fraction(liftToJet(n, dim), liftToJet(d, dim))
    case Root(deg, rad)                 => Root(deg.map(liftToJet(_, dim)), liftToJet(rad, dim))
    case Group(e)                       => Group(liftToJet(e, dim))
    case ExprSeq(es)                    => ExprSeq(es.map(liftToJet(_, dim)))
    case BracketGroup(o, c, e)          => BracketGroup(o, c, liftToJet(e, dim))
    case Superscript(b, s)              => Superscript(liftToJet(b, dim), liftToJet(s, dim))
    case Subscript(b, s)                => Subscript(liftToJet(b, dim), liftToJet(s, dim))
    case Operator(s)                    => Operator(s)
    case TextNode(c)                    => TextNode(c)
    case SubSup(b, sub, sup)            => SubSup(liftToJet(b, dim), liftToJet(sub, dim), liftToJet(sup, dim))
    case Over(b, t)                     => Over(liftToJet(b, dim), liftToJet(t, dim))
    case Under(b, bot)                  => Under(liftToJet(b, dim), liftToJet(bot, dim))
    case Style(v, c)                    => Style(v, liftToJet(c, dim))
    case Enclose(n, c)                  => Enclose(n, liftToJet(c, dim))
    case Color(col, c)                  => Color(col, liftToJet(c, dim))
    case Sum(idx, lo, hi, body)         => Sum(liftToJet(idx, dim), liftToJet(lo, dim), liftToJet(hi, dim), liftToJet(body, dim))
    case Integral(v, lo, hi, body)      => Integral(liftToJet(v, dim), liftToJet(lo, dim), liftToJet(hi, dim), liftToJet(body, dim))
    case MathVector(elems)              => MathVector(elems.map(liftToJet(_, dim)))
    case Matrix(elems, r, c, rs, cs, o) => Matrix(elems.map(liftToJet(_, dim)), r, c, rs, cs, o)
end ForwardDiff
