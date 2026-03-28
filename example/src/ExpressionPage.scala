package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*

object ExpressionPage:
  def render(): HtmlElement =
    val asciiVar = Var("sqrt(x) + e^x")
    val asciiResult = asciiVar.signal.map { s =>
      mathlify.AsciiMath.translate(s)
    }
    val varMap = Var(Map.empty[String, Double])

    div(
      cls := "cards-grid",
      // Expression input card
      Card(_.withHeader := true)(
        cls := "expression-card",
        div(
          slot := "header",
          Icon()("function"),
          span(" Expression")
        ),
        p("Type an AsciiMath expression and see it rendered live:"),
        Textarea(
          _.label := "AsciiMath",
          _.placeholder := "e.g. sqrt(x^2 + 1)",
          _.rows := 3
        )(
          value <-- asciiVar.signal,
          onInput.mapToValue --> asciiVar.writer
        ),
        div(
          cls := "rendered-math",
          child <-- asciiResult.map { s =>
            s.map(mathlify.LaminarRenderer.render)
              .getOrElse(
                Callout(_.variant := "danger")(
                  "Invalid expression"
                ): HtmlElement
              )
          }
        )
      ),
      // Free variables card
      Card(_.withHeader := true)(
        cls := "variables-card",
        div(
          slot := "header",
          Icon()("sliders"),
          span(" Variables")
        ),
        p("Set values for free variables to evaluate the expression:"),
        div(
          cls := "variable-inputs",
          children <-- asciiResult.map { s =>
            s.fold(
              err =>
                Seq(
                  Callout(_.variant := "warning")(
                    s"Parse error: $err"
                  ): HtmlElement
                ),
              expr =>
                val vars = mathlify.Evaluator.unboundVars(expr).toSeq
                if vars.isEmpty then Seq(Callout(_.variant := "success")("No free variables — expression is fully evaluable!"): HtmlElement)
                else
                  vars.map { v =>
                    Input(
                      _.label := s"$v",
                      _.placeholder := "0",
                      _.tpe := "number"
                    )(
                      onInput.mapToValue --> { value =>
                        varMap.update { m =>
                          m + (v -> mathlify.Evaluator.parseConstant(value).getOrElse(Double.NaN))
                        }
                      }
                    ): HtmlElement
                  }
            )
          }
        )
      ),
      // Evaluation result card
      Card(_.withHeader := true)(
        cls := "eval-card",
        div(
          slot := "header",
          Icon()("equals"),
          span(" Result")
        ),
        p("The evaluated (or partially reduced) result:"),
        div(
          cls := "eval-result",
          child <-- asciiResult.combineWith(varMap.signal).map { case (s, vars) =>
            s.fold(
              err => span(s"Parse error"): HtmlElement,
              expr =>
                mathlify.Evaluator.partialEval(expr, vars) match
                  case mathlify.EvalError(msg) =>
                    Callout(_.variant := "danger")(s"Error: $msg"): HtmlElement
                  case mathlify.Numeric(value) =>
                    Callout(_.variant := "success")(
                      span(cls := "numeric-result", s"= $value")
                    ): HtmlElement
                  case mathlify.PartiallyReduced(reducedExpr) =>
                    div(
                      Callout(_.variant := "neutral")(
                        "Partially reduced (some variables unbound):"
                      ),
                      div(cls := "rendered-math", mathlify.LaminarRenderer.render(reducedExpr))
                    ): HtmlElement
            )
          }
        )
      )
    )
  end render
end ExpressionPage
