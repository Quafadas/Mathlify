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
  case object Rekenrek extends Page
  case object ArrayBoard extends Page
  case class Matrix(a: String, b: String) extends Page
  object Matrix:
    val default: Matrix = Matrix("[(1,2,3),(4,5,6)]", "[(7,8),(9,10),(11,12)]")
  end Matrix
  case object Clock extends Page
  case object Dijkstra extends Page
  case object AutoDiff extends Page
  case object ReverseAD extends Page
end Page

// Compute fragment base path dynamically so it works at any sub-path (e.g. /Mathlify/ on GitHub Pages)
lazy val appBasePath: String =
  val path = dom.document.location.pathname
  val dir =
    if path.endsWith("/") then path.dropRight(1)
    else path.substring(0, path.lastIndexOf('/'))
  dir + "/#"
end appBasePath

val homeRoute = Route.static(Page.Home, root / "home", basePath = appBasePath)
val expressionRoute =
  Route.static(Page.Expression, root / "expression", basePath = appBasePath)
val quadraticRoute =
  Route.static(Page.Quadratic, root / "quadratic", basePath = appBasePath)
val clockRoute = Route.static(Page.Clock, root / "clock", basePath = appBasePath)
val dijkstraRoute =
  Route.static(Page.Dijkstra, root / "dijkstra", basePath = appBasePath)
val rekenrekRoute =
  Route.static(Page.Rekenrek, root / "rekenrek", basePath = appBasePath)
val arrayBoardRoute =
  Route.static(Page.ArrayBoard, root / "arrayboard", basePath = appBasePath)
val autoDiffRoute =
  Route.static(Page.AutoDiff, root / "autodiff", basePath = appBasePath)
val reverseADRoute =
  Route.static(Page.ReverseAD, root / "reversead", basePath = appBasePath)
val matrixRoute = Route.onlyQuery[Page.Matrix, (Option[String], Option[String])](
  encode = page => (Some(page.a), Some(page.b)),
  decode = args =>
    Page.Matrix(
      a = args._1.getOrElse(Page.Matrix.default.a),
      b = args._2.getOrElse(Page.Matrix.default.b)
    ),
  pattern = (root / "matrix") ? (param[String]("a").? & param[String]("b").?),
  basePath = appBasePath
)

object router
    extends Router[Page](
      routes = List(homeRoute, expressionRoute, quadraticRoute, rekenrekRoute, arrayBoardRoute, autoDiffRoute, reverseADRoute, clockRoute, dijkstraRoute, matrixRoute),
      serializePage = {
        case Page.Home         => "Home"
        case Page.Expression   => "Expression"
        case Page.Quadratic    => "Quadratic"
        case Page.Clock        => "Clock"
        case Page.Dijkstra     => "Dijkstra"
        case Page.Rekenrek     => "Rekenrek"
        case Page.ArrayBoard   => "ArrayBoard"
        case Page.AutoDiff     => "AutoDiff"
        case Page.ReverseAD    => "ReverseAD"
        case Page.Matrix(a, b) => s"Matrix\u0000$a\u0000$b"
      },
      deserializePage = {
        case "Home"                            => Page.Home
        case "Expression"                      => Page.Expression
        case "Quadratic"                       => Page.Quadratic
        case "Clock"                           => Page.Clock
        case "Dijkstra"                        => Page.Dijkstra
        case "Rekenrek"                        => Page.Rekenrek
        case "ArrayBoard"                      => Page.ArrayBoard
        case "AutoDiff"                        => Page.AutoDiff
        case "ReverseAD"                       => Page.ReverseAD
        case s if s.startsWith("Matrix\u0000") =>
          val rest = s.stripPrefix("Matrix\u0000")
          val sep = rest.indexOf('\u0000')
          if sep >= 0 then Page.Matrix(rest.substring(0, sep), rest.substring(sep + 1))
          else Page.Matrix.default
          end if
      },
      getPageTitle = {
        case Page.Home       => "Mathlify"
        case Page.Expression => "Expression Explorer – Mathlify"
        case Page.Quadratic  => "Quadratic Formula – Mathlify"
        case Page.Clock      => "Clock Arithmetic – Mathlify"
        case Page.Dijkstra   => "Dijkstra's Algorithm – Mathlify"
        case Page.Rekenrek   => "Rekenrek – Mathlify"
        case Page.ArrayBoard => "Array Board Game – Mathlify"
        case Page.AutoDiff   => "Automatic Differentiation – Mathlify"
        case Page.ReverseAD  => "Reverse-Mode AD – Mathlify"
        case _: Page.Matrix  => "Matrix Multiplication – Mathlify"
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
    .collectStatic(Page.Clock)(ClockPage.render())
    .collectStatic(Page.Dijkstra)(DijkstraPage.render())
    .collectStatic(Page.Rekenrek)(RekenrekPage.render())
    .collectStatic(Page.ArrayBoard)(ArrayBoardPage.render())
    .collectStatic(Page.AutoDiff)(AutoDiffPage.render())
    .collectStatic(Page.ReverseAD)(ReverseADPage.render())
    .collectSignal[Page.Matrix](sig => MatrixPage.render(sig))

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
      p(cls := "subtitle", "A library of fun maths")
    ),
    child <-- splitter.signal
  )
end app
