package mathlify

/** Provides the `mathlify` string interpolator.
  *
  * Usage:
  * {{{
  *   import mathlify.*
  *
  *   val expr: MathExpr = mathlify"(a + b) + c = a + (b + c)"
  * }}}
  *
  * Throws [[MathParseException]] at runtime if the expression cannot be parsed.
  */
extension (sc: StringContext)
  def mathlify(args: Any*): MathExpr =
    MathParser.parse(sc.s(args*))
