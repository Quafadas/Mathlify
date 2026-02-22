package mathlify

/** The AST for mathematical expressions parsed by [[MathParser]]. */
enum MathExpr derives CanEqual:
  case Var(name: String)
  case Num(value: String)
  case Add(left: MathExpr, right: MathExpr)
  case Sub(left: MathExpr, right: MathExpr)
  case Mul(left: MathExpr, right: MathExpr)
  case Div(left: MathExpr, right: MathExpr)
  case Eq(left: MathExpr, right: MathExpr)
  case Parens(inner: MathExpr)
