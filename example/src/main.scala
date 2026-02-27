import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main def entryPt(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    app
  )

def app =
  val simpleVar = Var("x + 2")
  val asciiVar = Var("sqrt(x)")
  var asciiResult = asciiVar.signal.map { s =>
    mathlify.AsciiMath.translate(s)
  }
  val varMap = Var(Map.empty[String, Double])
  div(    
    h1(s"Ascii Parser"),
    // https://demo.laminar.dev/app/form/controlled-inputs
    p("Enter an AsciiMath expression:"),
    input(
      typ := "text",
      controlled(
        value <-- asciiVar.signal,
        onInput.mapToValue --> asciiVar.writer
      )
    ),
    p(child <-- asciiResult.map { s =>
      s
        .map(mathlify.LaminarRenderer.render)
        .getOrElse(div("Invalid expression"))

    }),
    h1("Free Variables"),
    p("Set these variables in the context to see if the expression can be evaluated:"),
    div(
      children <-- asciiResult.map { s =>
        s.fold(
          err => Seq(p(s"Invalid expression - see above: $err")),
          expr =>
            mathlify.Evaluator.unboundVars(expr).toSeq.map { v =>
              div(
                s"$v = ",
                input(
                  typ := "text",
                  onInput.mapToValue --> { value =>
                    varMap.update { m =>
                      m + (v -> value.toDoubleOption.getOrElse(0.0))
                    }
                  }
                )
              )
            }
          )
      }              
    ),
    h1("Eval"),
    p("We attempt to evaludate the expression, and if it contains free variables, we just show the reduced form."),
    p(
      "Can eval :",
      child <-- asciiResult.combineWith(varMap.signal).map { case (s, vars) =>
        s.fold(
          err => s"No - $err",
          expr =>
            mathlify.Evaluator.isEvaluable(expr, vars) match
              case true  => s"Yes"
              case false => s"No - ${mathlify.Evaluator.eval(expr)}"
        )
      }
    ),
    p(child <-- asciiResult.combineWith(varMap.signal).map { case (s, vars) =>
      s.fold(
        err => s"Invalid expression - see above",
        expr =>
          mathlify.Evaluator.eval(expr, vars) match
            case mathlify.EvalError(msg)                => s"Eval error: $msg"
            case mathlify.Numeric(value)                => s"Numeric result: $value"
            case mathlify.PartiallyReduced(reducedExpr) => s"Partially reduced: ${mathlify.LaminarRenderer.render(reducedExpr)}"
      )
    }),
    h1("Partial Eval"),
    div(children <-- asciiResult.combineWith(varMap.signal).map { case (s, vars) =>
      s.fold(
        err => Seq(p(s"Invalid expression - see above: $err")),
        expr =>
          mathlify.Evaluator.partialEval(expr, vars) match
            case mathlify.EvalError(msg)                => Seq(p(s"Eval error: $msg"))
            case mathlify.Numeric(value)                => Seq(p(s"Numeric result: $value"))
            case mathlify.PartiallyReduced(reducedExpr) => Seq(p("Partially reduced:"), mathlify.LaminarRenderer.render(reducedExpr))
      )
    })
  )
end app
