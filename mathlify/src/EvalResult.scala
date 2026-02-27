package mathlify

// ── Result types ──────────────────────────────────────────────────────────

sealed trait EvalResult
case class Numeric(value: Double) extends EvalResult
case class PartiallyReduced(expr: MathExpr) extends EvalResult
case class EvalError(message: String) extends EvalResult
