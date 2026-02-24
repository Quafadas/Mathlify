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
    
    child <-- asciiVar.signal.map{ s => 
      mathlify.AsciiMath.translate(s)
        .map(mathlify.LaminarRenderer.render)
        .getOrElse(div("Invalid expression"))
    }
  )