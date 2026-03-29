package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*

object HomePage:
  def render(): HtmlElement =
    div(
      cls := "cards-grid",
      Card(_.withHeader := true)(
        cls := "home-card",
        div(
          slot := "header",
          Icon()("function"),
          span(" Expression Explorer")
        ),
        p("Type any AsciiMath expression and watch it render live. Bind variables and evaluate — a mathematical playground."),
        Button()(
          "Open Explorer",
          onClick --> (_ => router.pushState(Page.Expression))
        )
      ),
      Card(_.withHeader := true)(
        cls := "home-card",
        div(
          slot := "header",
          Icon()("square-root-alt"),
          span(" The Quadratic Formula")
        ),
        p("A step-by-step proof of the quadratic formula by completing the square — then solve your own equations interactively."),
        Button()(
          "Explore Proof",
          onClick --> (_ => router.pushState(Page.Quadratic))
        )
      ),
      Card(_.withHeader := true)(
        cls := "home-card",
        div(
          slot := "header",
          Icon()("grid"),
          span(" Matrix Multiplication")
        ),
        p("See how matrix multiplication works cell by cell. Hover to highlight rows and columns, click output cells to reveal the dot-product calculation."),
        Button()(
          "Try It",
          onClick --> (_ => router.pushState(Page.Matrix))
        )
      )
    )
end HomePage
