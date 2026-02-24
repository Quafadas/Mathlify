package mathlify

import MathExpr.*

/** A simple AsciiMath-subset translator that preprocesses AsciiMath-specific
  * syntax and delegates to MathParser.
  *
  * AsciiMath differences handled:
  *   - `_( … )` subscript/superscript grouping → `_{ … }`
  *   - `^( … )` superscript grouping            → `^{ … }`
  *   - `RR`                                     → Constant("ℝ")
  */
object AsciiMath:

  def translate(input: String): Either[String, MathExpr] =
    val preprocessed = preprocess(input)
    MathParser.parse(preprocessed).map(postProcess)

  // ── preprocessing ─────────────────────────────────────────────────────────

  /** Replace `_(…)` / `^(…)` with `_{…}` / `^{…}`, and `RR` with the
    * internal token `mathlifyRR`.
    */
  private def preprocess(input: String): String =
    val sb = new StringBuilder
    var i  = 0
    while i < input.length do
      if i + 1 < input.length &&
        (input(i) == '_' || input(i) == '^') &&
        input(i + 1) == '('
      then
        sb += input(i) // keep _ or ^
        sb += '{'
        var depth = 1
        i += 2
        while i < input.length && depth > 0 do
          input(i) match
            case '(' =>
              depth += 1; sb += '('; i += 1
            case ')' =>
              depth -= 1
              if depth > 0 then sb += ')' else sb += '}'
              i += 1
            case c =>
              sb += c; i += 1
      else
        sb += input(i)
        i += 1
    sb.toString.replaceAll("\\bRR\\b", "mathlifyRR")

  // ── post-processing ───────────────────────────────────────────────────────

  /** Convert internal marker symbols to proper AST nodes. */
  private def postProcess(expr: MathExpr): MathExpr = expr match
    case Symbol("mathlifyRR")         => Constant("ℝ")
    case Number(_) | Symbol(_) | Constant(_) => expr
    case Add(l, r)                    => Add(postProcess(l), postProcess(r))
    case Sub(l, r)                    => Sub(postProcess(l), postProcess(r))
    case Mul(l, r)                    => Mul(postProcess(l), postProcess(r))
    case Div(l, r)                    => Div(postProcess(l), postProcess(r))
    case Pow(b, e)                    => Pow(postProcess(b), postProcess(e))
    case Neg(e)                       => Neg(postProcess(e))
    case FunctionCall(n, as)          => FunctionCall(n, as.map(postProcess))
    case Fraction(n, d)               => Fraction(postProcess(n), postProcess(d))
    case Root(deg, rad)               => Root(deg.map(postProcess), postProcess(rad))
    case Sum(idx, lo, hi, body)       => Sum(postProcess(idx), postProcess(lo), postProcess(hi), postProcess(body))
    case Integral(v, lo, hi, body)    => Integral(postProcess(v), postProcess(lo), postProcess(hi), postProcess(body))
    case Group(e)                     => Group(postProcess(e))
    case MathVector(els)              => MathVector(els.map(postProcess))
    case Matrix(els, r, c, rs, cs, o) => Matrix(els.map(postProcess), r, c, rs, cs, o)
    case Subscript(b, s)              => Subscript(postProcess(b), postProcess(s))
    case Superscript(b, s)            => Superscript(postProcess(b), postProcess(s))
