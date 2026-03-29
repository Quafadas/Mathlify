package mathlify.example

import com.raquo.laminar.api.L.*
import com.raquo.waypoint.*
import org.scalajs.dom
import io.github.nguyenyou.webawesome.laminar.*

sealed trait Page
object Page:
  case object Home extends Page
  case object Expression extends Page
  case object Quadratic extends Page
  case object Matrix extends Page
end Page

val homeRoute = Route.static(Page.Home, root / "home", basePath = Route.fragmentBasePath)
val expressionRoute =
  Route.static(Page.Expression, root / "expression", basePath = Route.fragmentBasePath)
val quadraticRoute =
  Route.static(Page.Quadratic, root / "quadratic", basePath = Route.fragmentBasePath)
val matrixRoute =
  Route.static(Page.Matrix, root / "matrix", basePath = Route.fragmentBasePath)

object router
    extends Router[Page](
      routes = List(homeRoute, expressionRoute, quadraticRoute, matrixRoute),
      serializePage = {
        case Page.Home       => "Home"
        case Page.Expression => "Expression"
        case Page.Quadratic  => "Quadratic"
        case Page.Matrix     => "Matrix"
      },
      deserializePage = {
        case "Home"       => Page.Home
        case "Expression" => Page.Expression
        case "Quadratic"  => Page.Quadratic
        case "Matrix"     => Page.Matrix
      },
      getPageTitle = {
        case Page.Home       => "Mathlify"
        case Page.Expression => "Expression Explorer – Mathlify"
        case Page.Quadratic  => "Quadratic Formula – Mathlify"
        case Page.Matrix     => "Matrix Multiplication – Mathlify"
      },
      routeFallback = _ => Page.Home
    )

@main def entryPt(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    app
  )

def app =
  val splitter = SplitRender[Page, HtmlElement](router.currentPageSignal)
    .collectStatic(Page.Home)(HomePage.render())
    .collectStatic(Page.Expression)(ExpressionPage.render())
    .collectStatic(Page.Quadratic)(QuadraticPage.render())
    .collectStatic(Page.Matrix)(MatrixPage.render())

  div(
    cls := "page-container",
    div(
      cls := "page-header",
      a(
        cls := "header-link",
        router.navigateTo(Page.Home),
        Icon()("calculator", cls := "header-icon"),
        h1("Mathlify")
      ),
      p(cls := "subtitle", "An educational library of fun maths")
    ),
    child <-- splitter.signal
  )
end app
