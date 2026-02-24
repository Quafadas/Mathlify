package mathlify

sealed trait MathExpr

object MathExpr:
  // Atoms
  case class Number(value: Double) extends MathExpr
  case class Symbol(name: String) extends MathExpr
  case class Constant(name: String) extends MathExpr

  // Operators
  case class Add(lhs: MathExpr, rhs: MathExpr) extends MathExpr
  case class Sub(lhs: MathExpr, rhs: MathExpr) extends MathExpr
  case class Mul(lhs: MathExpr, rhs: MathExpr) extends MathExpr
  case class Div(lhs: MathExpr, rhs: MathExpr) extends MathExpr
  case class Pow(base: MathExpr, exponent: MathExpr) extends MathExpr
  case class Neg(expr: MathExpr) extends MathExpr

  // Structures
  case class FunctionCall(name: String, args: List[MathExpr]) extends MathExpr
  case class Fraction(numerator: MathExpr, denominator: MathExpr) extends MathExpr
  case class Root(degree: Option[MathExpr], radicand: MathExpr) extends MathExpr
  case class Sum(index: MathExpr, lower: MathExpr, upper: MathExpr, body: MathExpr) extends MathExpr
  case class Integral(variable: MathExpr, lower: MathExpr, upper: MathExpr, body: MathExpr)
      extends MathExpr
  case class Group(expr: MathExpr) extends MathExpr

  // Collections
  case class MathVector(elements: List[MathExpr]) extends MathExpr
  case class Matrix(
      elements: List[MathExpr],
      rows: Int,
      cols: Int,
      rowStride: Int,
      colStride: Int,
      offset: Int
  ) extends MathExpr

  // Annotations
  case class Subscript(base: MathExpr, sub: MathExpr) extends MathExpr
  case class Superscript(base: MathExpr, sup: MathExpr) extends MathExpr
