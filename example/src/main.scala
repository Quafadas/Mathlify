package mathlify.example

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import io.github.nguyenyou.webawesome.laminar.*

enum Page:
  case Home, Expression, Quadratic, Matrix

object Router:
  val currentPage: Var[Page] = Var(Page.Home)

@main def entryPt(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    app
  )

def app =
  div(
    cls := "page-container",
    div(
      cls := "page-header",
      a(
        cls := "header-link",
        onClick.preventDefault.mapTo(Page.Home) --> Router.currentPage.writer,
        href := "#",
        Icon()("calculator", cls := "header-icon"),
        h1("Mathlify")
      ),
      p(cls := "subtitle", "An educational library of fun maths")
    ),
    child <-- Router.currentPage.signal.map {
      case Page.Home       => HomePage.render()
      case Page.Expression => ExpressionPage.render()
      case Page.Quadratic  => QuadraticPage.render()
      case Page.Matrix     => MatrixPage.render()
    }
  )
end app
