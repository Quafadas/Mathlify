package mathlify

import com.raquo.laminar.api.L.*

/** Renders a [[MathExpr]] AST into a tree of Laminar [[HtmlElement]]s.
  *
  * Each node is wrapped in a `<span>` with a semantic CSS class so the DOM
  * can be styled or inspected by tests.
  *
  * Example:
  * {{{
  *   import mathlify.*
  *   val el = MathComponent.render(mathlify"(a + b) + c = a + (b + c)")
  *   render(dom.document.querySelector("#app"), el)
  * }}}
  */
object MathComponent:

  def render(expr: MathExpr): HtmlElement =
    expr match
      case MathExpr.Var(name) =>
        span(cls := "math-var", name)

      case MathExpr.Num(value) =>
        span(cls := "math-num", value)

      case MathExpr.Add(l, r) =>
        span(cls := "math-add", render(l), span(cls := "math-op math-plus", " + "), render(r))

      case MathExpr.Sub(l, r) =>
        span(cls := "math-sub", render(l), span(cls := "math-op math-minus", " - "), render(r))

      case MathExpr.Mul(l, r) =>
        span(cls := "math-mul", render(l), span(cls := "math-op math-times", " \u00b7 "), render(r))

      case MathExpr.Div(l, r) =>
        span(cls := "math-div", render(l), span(cls := "math-op math-div-sign", " \u00f7 "), render(r))

      case MathExpr.Eq(l, r) =>
        span(cls := "math-equation", render(l), span(cls := "math-op math-equals", " = "), render(r))

      case MathExpr.Parens(inner) =>
        span(
          cls := "math-parens",
          span(cls := "math-paren math-paren-open", "("),
          render(inner),
          span(cls := "math-paren math-paren-close", ")")
        )
