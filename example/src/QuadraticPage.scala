package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*

object QuadraticPage:

  private def mathml(ascii: String): HtmlElement =
    mathlify.AsciiMath.translate(ascii) match
      case Right(expr) => mathlify.LaminarRenderer.render(expr)
      case Left(_)     => span(ascii)

  // ── Proof section ─────────────────────────────────────────────────────────

  private def proofSection(): HtmlElement =
    div(
      cls := "proof-section",
      h2("Deriving the Quadratic Formula"),
      p("We start with the general quadratic equation and solve for ", i("x"), " by completing the square."),
      proofStep("Step 1: Start with the general form", "ax^2 + bx + c = 0", "where a ≠ 0."),
      proofStep("Step 2: Divide both sides by a", "x^2 + b/a x + c/a = 0", ""),
      proofStep("Step 3: Move the constant term to the right", "x^2 + b/a x = -c/a", ""),
      proofStep(
        "Step 4: Complete the square",
        "x^2 + b/a x + (b/(2a))^2 = -c/a + (b/(2a))^2",
        "Add (b/(2a))² to both sides."
      ),
      proofStep(
        "Step 5: Factor the left side",
        "(x + b/(2a))^2 = -c/a + b^2/(4a^2)",
        "The left side is now a perfect square."
      ),
      proofStep(
        "Step 6: Combine the right side",
        "(x + b/(2a))^2 = (b^2 - 4ac)/(4a^2)",
        "Common denominator 4a²."
      ),
      proofStep(
        "Step 7: Take the square root",
        "x + b/(2a) = +- sqrt(b^2 - 4ac)/(2a)",
        ""
      ),
      proofStep(
        "Step 8: Isolate x",
        "x = (-b +- sqrt(b^2 - 4ac))/(2a)",
        ""
      ),
      Callout(_.variant := "success")(
        cls := "qed-callout",
        "∎  This is the quadratic formula."
      )
    )

  private def proofStep(title: String, ascii: String, note: String): HtmlElement =
    div(
      cls := "proof-step",
      h3(title),
      div(cls := "rendered-math", mathml(ascii)),
      if note.nonEmpty then p(cls := "proof-note", note) else emptyNode
    )

  // ── Solver section ────────────────────────────────────────────────────────

  private def solverSection(): HtmlElement =
    val aVar = Var("1")
    val bVar = Var("-5")
    val cVar = Var("6")

    val solution = aVar.signal
      .combineWith(bVar.signal, cVar.signal)
      .map { case (as, bs, cs) =>
        for
          a <- as.toDoubleOption if a != 0
          b <- bs.toDoubleOption
          c <- cs.toDoubleOption
        yield solve(a, b, c)
      }

    div(
      cls := "solver-section",
      h2("Interactive Solver"),
      p("Enter coefficients and watch the step-by-step solution unfold."),
      div(
        cls := "solver-inputs",
        coeffInput("a", aVar),
        coeffInput("b", bVar),
        coeffInput("c", cVar)
      ),
      div(
        cls := "solver-equation",
        child <-- aVar.signal.combineWith(bVar.signal, cVar.signal).map { case (a, b, c) =>
          mathml(s"${a}x^2 + (${b})x + (${c}) = 0")
        }
      ),
      div(
        child <-- solution.map {
          case None         => Callout(_.variant := "warning")("Enter valid coefficients (a \u2260 0)."): HtmlElement
          case Some(result) =>
            div(
              Callout(_.variant := "success")(
                cls := "answer-callout",
                strong("Answer: "),
                div(cls := "rendered-math", mathml(result.answer))
              ),
              div(cls := "solver-steps", renderSteps(result.steps))
            )
        }
      )
    )
  end solverSection

  private def coeffInput(label: String, v: Var[String]): HtmlElement =
    Input(
      _.label := label,
      _.tpe := "number"
    )(
      value <-- v.signal,
      onInput.mapToValue --> v.writer,
      cls := "coeff-input"
    )

  // ── Solver logic ──────────────────────────────────────────────────────────

  case class SolveStep(title: String, ascii: String, note: String)
  case class SolveResult(answer: String, steps: List[SolveStep])

  private def fmt(d: Double): String =
    if d == d.toLong.toDouble then d.toLong.toString
    else f"$d%.4g"

  private def solve(a: Double, b: Double, c: Double): SolveResult =
    val disc = b * b - 4 * a * c
    val steps = List.newBuilder[SolveStep]

    steps += SolveStep(
      "Original equation",
      s"${fmt(a)}x^2 + (${fmt(b)})x + (${fmt(c)}) = 0",
      ""
    )

    steps += SolveStep(
      "Identify coefficients",
      s"a = ${fmt(a)}, \\ b = ${fmt(b)}, \\ c = ${fmt(c)}",
      ""
    )

    steps += SolveStep(
      "Compute the discriminant",
      s"Delta = b^2 - 4ac = (${fmt(b)})^2 - 4(${fmt(a)})(${fmt(c)}) = ${fmt(disc)}",
      ""
    )

    val answer: String =
      if disc < 0 then
        steps += SolveStep(
          "Discriminant is negative",
          s"Delta = ${fmt(disc)} < 0",
          "No real solutions — the roots are complex."
        )
        val real = -b / (2 * a)
        val imag = math.sqrt(-disc) / (2 * a)
        steps += SolveStep(
          "Complex roots",
          s"x = ${fmt(real)} +- ${fmt(imag)}i",
          ""
        )
        s"x = ${fmt(real)} +- ${fmt(imag)}i"
      else if disc == 0 then
        val x = -b / (2 * a)
        steps += SolveStep(
          "Discriminant is zero — one repeated root",
          s"x = -b/(2a) = ${fmt(-b)}/(2 * ${fmt(a)}) = ${fmt(x)}",
          ""
        )
        s"x = ${fmt(x)}"
      else
        val sqrtDisc = math.sqrt(disc)
        steps += SolveStep(
          "Take the square root of the discriminant",
          s"sqrt(Delta) = sqrt(${fmt(disc)}) = ${fmt(sqrtDisc)}",
          ""
        )
        val x1 = (-b + sqrtDisc) / (2 * a)
        val x2 = (-b - sqrtDisc) / (2 * a)
        steps += SolveStep(
          "Apply the quadratic formula",
          s"x = (-b +- sqrt(Delta))/(2a)",
          ""
        )
        steps += SolveStep(
          "First root",
          s"x_1 = (${fmt(-b)} + ${fmt(sqrtDisc)})/(${fmt(2 * a)}) = ${fmt(x1)}",
          ""
        )
        steps += SolveStep(
          "Second root",
          s"x_2 = (${fmt(-b)} - ${fmt(sqrtDisc)})/(${fmt(2 * a)}) = ${fmt(x2)}",
          ""
        )
        s"x_1 = ${fmt(x1)}, \\ x_2 = ${fmt(x2)}"

    SolveResult(answer, steps.result())
  end solve

  private def renderSteps(steps: List[SolveStep]): HtmlElement =
    div(
      steps.zipWithIndex.map { case (s, i) =>
        div(
          cls := "proof-step solver-step",
          h3(s"${i + 1}. ${s.title}"),
          div(cls := "rendered-math", mathml(s.ascii)),
          if s.note.nonEmpty then p(cls := "proof-note", s.note) else emptyNode
        )
      }
    )

  // ── Top-level render ──────────────────────────────────────────────────────

  def render(): HtmlElement =
    div(
      proofSection(),
      Divider()(),
      solverSection()
    )
end QuadraticPage
