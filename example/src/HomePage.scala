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
          onClick --> (_ => router.pushState(Page.Matrix.default))
        )
      ),
      Card(_.withHeader := true)(
        cls := "home-card",
        div(
          slot := "header",
          Icon()("clock"),
          span(" Clock Arithmetic")
        ),
        p("Explore modular arithmetic visually. Watch numbers wrap around a clock, discover cycles, and reveal hidden star-polygon patterns through multiplication."),
        Button()(
          "Explore Clocks",
          onClick --> (_ => router.pushState(Page.Clock))
        )
      ),
      Card(_.withHeader := true)(
        cls := "home-card",
        div(
          slot := "header",
          Icon()("calculator"),
          span(" Rekenrek")
        ),
        p("An interactive counting frame for exploring numbers and addition. Push the coloured beads to the left to count them — great for building number sense!"),
        Button()(
          "Start Counting",
          onClick --> (_ => router.pushState(Page.Rekenrek))
        )
      ),
      Card(_.withHeader := true)(
        cls := "home-card",
        div(
          slot := "header",
          Icon()("grid"),
          span(" Array Board Game")
        ),
        p(
          "Build rectangular arrays on a 10×10 grid! Place arrays to score points, find multiple ways to make a target number, and discover multiplication through hands-on exploration."
        ),
        Button()(
          "Play Now",
          onClick --> (_ => router.pushState(Page.ArrayBoard))
        )
      )
    )
end HomePage
