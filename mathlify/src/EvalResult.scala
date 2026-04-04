package mathlify

// ── Result types ──────────────────────────────────────────────────────────

sealed trait EvalResult[+A]
case class Numeric[A](value: A) extends EvalResult[A]
case class PartiallyReduced[A](expr: MathExpr[A]) extends EvalResult[A]
case class EvalError(message: String) extends EvalResult[Nothing]

/** Convenience alias for the common Double-specialised result. */
type EvalResultD = EvalResult[Double]
