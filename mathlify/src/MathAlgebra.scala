package mathlify

// ── Type class hierarchy ─────────────────────────────────────────────────────

/** Minimum algebra: addition, subtraction, multiplication, negation, and lifting literals. */
trait MathRing[A]:
  def zero: A
  def one: A
  def fromLong(n: Long): A
  def fromDouble(d: Double): A
  def plus(a: A, b: A): A
  def minus(a: A, b: A): A
  def times(a: A, b: A): A
  def negate(a: A): A
end MathRing

/** Adds division. Not valid for all types (e.g. non-square matrices). */
trait MathField[A] extends MathRing[A]:
  def div(a: A, b: A): A
end MathField

/** Adds real-valued power. */
trait MathPow[A] extends MathField[A]:
  def pow(a: A, b: A): A
end MathPow

/** Full transcendental / trigonometric algebra. */
trait MathTrig[A] extends MathPow[A]:
  def sin(a: A): A
  def cos(a: A): A
  def tan(a: A): A
  def exp(a: A): A
  def log(a: A): A
  def sqrt(a: A): A
end MathTrig

object MathTrig:
  given MathTrig[Double] with
    val zero = 0.0
    val one = 1.0
    def fromLong(n: Long) = n.toDouble
    def fromDouble(d: Double) = d
    def plus(a: Double, b: Double) = a + b
    def minus(a: Double, b: Double) = a - b
    def times(a: Double, b: Double) = a * b
    def negate(a: Double) = -a
    def div(a: Double, b: Double) = a / b
    def pow(a: Double, b: Double) = scala.math.pow(a, b)
    def sin(a: Double) = scala.math.sin(a)
    def cos(a: Double) = scala.math.cos(a)
    def tan(a: Double) = scala.math.tan(a)
    def exp(a: Double) = scala.math.exp(a)
    def log(a: Double) = scala.math.log(a)
    def sqrt(a: Double) = scala.math.sqrt(a)
  end given
end MathTrig

/** Converts a value of type A to a display string. */
trait MathShow[-A]:
  def show(a: A): String
end MathShow

object MathShow:
  /** Default `Double` display: whole numbers rendered without a decimal point (e.g. `3` instead of `3.0`) for cleaner mathematical output.
    */
  given MathShow[Double] with
    def show(a: Double): String =
      if a % 1 == 0 && !a.isInfinite then a.toLong.toString else a.toString
  end given
end MathShow
