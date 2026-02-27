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
  var asciiResult = asciiVar.signal.map{ s => 
    mathlify.AsciiMath.translate(s)
  }
  div(
    h1(
      s"Simple Parser",
    ),    
    // https://demo.laminar.dev/app/form/controlled-inputs
    input(
      typ := "text",
      controlled(
        value <-- simpleVar.signal,
        onInput.mapToValue --> simpleVar.writer
      )
    ),
    
    child <-- simpleVar.signal.map{ s => 
      mathlify.MathParser.parse(s)
        .map(mathlify.LaminarRenderer.render)
        .getOrElse(div("Invalid expression"))
    },
    h1(
      s"Ascii Parser",
    ),    
    // https://demo.laminar.dev/app/form/controlled-inputs
    input(
      typ := "text",
      controlled(
        value <-- asciiVar.signal,
        onInput.mapToValue --> asciiVar.writer
      )
    ),
    
    p(
      child <-- asciiResult.map{ s => 
        s
          .map(mathlify.LaminarRenderer.render)
          .getOrElse(div("Invalid expression"))

    }),
    h1("Eval"),
    p("We attempt to evaludate the expression, and if it contains free variables, we just show the reduced form."),
    p("Can eval :" , child <-- asciiResult.map{ s =>       
        s.fold(
          err => s"No - $err", 
          expr => mathlify.Evaluator.isEvaluable(expr, Map.empty) match
            case true => s"Yes"
            case false => s"No - ${mathlify.Evaluator.eval(expr)}"
        )
    }),
    p(child <-- asciiResult.map{ s =>       
        s.fold(
          err => s"Invalid expression - see above", 
          expr => mathlify.Evaluator.eval(expr) match
            case mathlify.EvalError(msg) => s"Eval error - $msg"
            case mathlify.Numeric(value) => s"Numeric result - $value"
            case mathlify.PartiallyReduced(reducedExpr) => s"Partially reduced - ${mathlify.LaminarRenderer.render(reducedExpr)}"
        )
    })

    

  )