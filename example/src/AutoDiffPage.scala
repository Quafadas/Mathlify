package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*

object AutoDiffPage:

  private def mathml(ascii: String): HtmlElement =
    mathlify.AsciiMath.translate(ascii) match
      case Right(expr) => mathlify.LaminarRenderer.render(expr)
      case Left(_)     => span(ascii)

  private def fmt(d: Double): String =
    if d == d.toLong.toDouble && !d.isInfinite then d.toLong.toString
    else f"$d%.6g"

  // ── Theory section ──────────────────────────────────────────────────────

  private def theorySection(): HtmlElement =
    div(
      cls := "theory-section",
      h2("What is Forward-Mode Automatic Differentiation?"),
      p(
        "Forward-mode AD computes derivatives automatically by augmenting every value with its derivative. ",
        "The key idea is the ",
        strong("dual number"),
        ": a pair ",
        mathml("(v, v')"),
        " where ",
        mathml("v"),
        " is the value and ",
        mathml("v'"),
        " is the derivative."
      ),
      p(
        "We seed the variable we are differentiating with respect to with ",
        mathml("(x, 1)"),
        " (derivative of ",
        mathml("x"),
        " with respect to itself is 1), ",
        "and all other variables and constants with ",
        mathml("(c, 0)"),
        ". Then we propagate through the computation using the standard differentiation rules."
      ),
      h3("Dual Number Arithmetic"),
      p("The rules for propagating derivatives through arithmetic operations:"),
      div(
        cls := "dual-rules",
        dualRule("Addition", "(a,a') + (b,b') = (a+b, a'+b')"),
        dualRule("Subtraction", "(a,a') - (b,b') = (a-b, a'-b')"),
        dualRule("Multiplication (Product Rule)", "(a,a') * (b,b') = (a*b, a'*b + a*b')"),
        dualRule("Division (Quotient Rule)", "(a,a') / (b,b') = (a/b, (a'*b - a*b') / b^2)")
      ),
      Divider()(),
      chainRuleSection(),
      Divider()(),
      quotientRuleSection()
    )

  private def dualRule(name: String, ascii: String): HtmlElement =
    div(
      cls := "dual-rule",
      strong(name, ": "),
      div(cls := "rendered-math", mathml(ascii))
    )

  // ── Chain rule ────────────────────────────────────────────────────────────

  private def chainRuleSection(): HtmlElement =
    div(
      cls := "chain-rule-section",
      h3("The Chain Rule"),
      p(
        "The chain rule tells us how to differentiate a composition of functions. ",
        "If ",
        mathml("y = f(g(x))"),
        ", then:"
      ),
      div(cls := "rendered-math highlight-formula", mathml("dy/dx = f'(g(x)) * g'(x)")),
      p(
        "In dual number terms, this is automatic! When we compute ",
        mathml("f"),
        " of a dual number ",
        mathml("(g, g')"),
        ", the result is ",
        mathml("(f(g), f'(g) * g')"),
        " — exactly the chain rule."
      ),
      h4("Example: ", mathml("e^(x^2)")),
      p("Let's trace through the chain rule with ", mathml("f(x) = e^(x^2)"), " at ", mathml("x = 1"), ":"),
      div(
        cls := "chain-rule-steps",
        proofStep("Seed", "x = (1, 1)", "Start with the dual number for x"),
        proofStep("Compute x²", "x^2 = (1, 1) * (1, 1) = (1, 1*1 + 1*1) = (1, 2)", "Product rule: (a,a')·(a,a') = (a², 2a·a')"),
        proofStep("Compute e^(x²)", "e^(x^2) = e^((1, 2)) = (e^1, e^1 * 2) = (e, 2e)", "Chain rule for exp: (eᵛ, eᵛ·v')"),
        proofStep("Result", "f(1) = e, \\ f'(1) = 2e", "The derivative of e^(x²) is 2x·e^(x²)")
      )
    )

  // ── Quotient rule ─────────────────────────────────────────────────────────

  private def quotientRuleSection(): HtmlElement =
    div(
      cls := "quotient-rule-section",
      h3("The Quotient Rule"),
      p(
        "For a quotient ",
        mathml("y = f(x)/g(x)"),
        ", the derivative is:"
      ),
      div(cls := "rendered-math highlight-formula", mathml("dy/dx = (f'(x)*g(x) - f(x)*g'(x)) / (g(x))^2")),
      p(
        "In dual numbers, division of ",
        mathml("(a, a')"),
        " by ",
        mathml("(b, b')"),
        " gives ",
        mathml("(a/b, (a'*b - a*b')/b^2)"),
        " — exactly the quotient rule."
      ),
      h4("Example: ", mathml("x/y")),
      p("Consider ", mathml("h(x,y) = x/y"), " and let's find ", mathml("(del h)/(del y)"), " at ", mathml("x = 4, y = 2"), ":"),
      div(
        cls := "quotient-rule-steps",
        proofStep("Seed", "x = (4, 0), \\ y = (2, 1)", "Differentiating with respect to y"),
        proofStep("Apply quotient rule", "(4, 0) / (2, 1) = (4/2, (0*2 - 4*1)/2^2) = (2, -1)", "(a'b − ab') / b²"),
        proofStep("Result", "h(4,2) = 2, \\ (del h)/(del y) = -1", "The partial derivative of x/y w.r.t. y is −x/y²")
      )
    )

  private def proofStep(title: String, ascii: String, note: String): HtmlElement =
    div(
      cls := "proof-step",
      h4(title),
      div(cls := "rendered-math", mathml(ascii)),
      if note.nonEmpty then p(cls := "proof-note", note) else emptyNode
    )

  // ── Worked examples section ───────────────────────────────────────────────

  private def examplesSection(): HtmlElement =
    div(
      cls := "examples-section",
      h2("Worked Examples"),
      workedExample(
        "Example 1: f(x) = x²",
        "x^2",
        "2x",
        Map("x" -> 3.0),
        "x",
        "The power rule gives d/dx(x²) = 2x. At x = 3: f(3) = 9, f'(3) = 6."
      ),
      workedExample(
        "Example 2: f(x) = eˣ",
        "e^x",
        "e^x",
        Map("x" -> 0.0),
        "x",
        "The exponential function is its own derivative. At x = 0: f(0) = 1, f'(0) = 1."
      ),
      workedExample(
        "Example 3: f(x) = e^(x²) — Chain Rule",
        "e^(x^2)",
        "2x * e^(x^2)",
        Map("x" -> 1.0),
        "x",
        "By the chain rule: d/dx(e^(x²)) = 2x·e^(x²). At x = 1: f(1) = e ≈ 2.718, f'(1) = 2e ≈ 5.436."
      ),
      h3("Partial Derivatives"),
      p("Forward-mode AD naturally computes partial derivatives. Seed the target variable with derivative 1 and all others with 0."),
      workedExample(
        "Example 4: ∂f/∂x where f(x,y) = x²y + y³",
        "x^2 * y + y^3",
        "2x * y",
        Map("x" -> 2.0, "y" -> 3.0),
        "x",
        "∂f/∂x = 2xy. At (2,3): f = 4·3 + 27 = 39, ∂f/∂x = 2·2·3 = 12."
      ),
      workedExample(
        "Example 5: ∂f/∂y where f(x,y) = x²y + y³",
        "x^2 * y + y^3",
        "x^2 + 3y^2",
        Map("x" -> 2.0, "y" -> 3.0),
        "y",
        "∂f/∂y = x² + 3y². At (2,3): f = 39, ∂f/∂y = 4 + 27 = 31."
      )
    )

  private def workedExample(
      title: String,
      exprAscii: String,
      derivAscii: String,
      env: Map[String, Double],
      wrt: String,
      explanation: String
  ): HtmlElement =
    val result = mathlify.AsciiMath.translate(exprAscii) match
      case Right(expr) => mathlify.ForwardDiff.differentiate(expr, env, wrt)
      case Left(_)     => Left("Parse error")

    val (fValue, fDeriv) = result match
      case Right((v, d)) => (fmt(v), fmt(d))
      case Left(msg)     => (s"Error: $msg", "Error")

    val envStr = env.map { case (k, v) => s"$k = ${fmt(v)}" }.mkString(", ")

    Card(_.withHeader := true)(
      cls := "worked-example",
      div(slot := "header", strong(title)),
      div(
        cls := "example-body",
        div(cls := "example-row", span(cls := "example-label", "Function: "), div(cls := "rendered-math", mathml(s"f = $exprAscii"))),
        div(
          cls := "example-row",
          span(cls := "example-label", s"Analytical derivative (w.r.t. $wrt): "),
          div(cls := "rendered-math", mathml(s"f' = $derivAscii"))
        ),
        div(cls := "example-row", span(cls := "example-label", s"Evaluated at: "), span(envStr)),
        div(
          cls := "example-results",
          Callout(_.variant := "success")(
            div(cls := "example-row", span("f = "), strong(fValue)),
            div(cls := "example-row", span(s"∂f/∂$wrt = "), strong(fDeriv))
          )
        ),
        p(cls := "proof-note", explanation)
      )
    )
  end workedExample

  // ── Interactive evaluator section ─────────────────────────────────────────

  private def evaluatorSection(): HtmlElement =
    val asciiVar = Var("x^2 * y + y^3")
    val asciiResult = asciiVar.signal.map(mathlify.AsciiMath.translate)
    val varMap = Var(Map.empty[String, Double])

    div(
      cls := "evaluator-section",
      h2("Interactive Derivative Evaluator"),
      p("Enter any expression, set variable values, and see ALL partial derivatives computed in a single pass."),
      div(
        cls := "cards-grid",
        // Expression input
        Card(_.withHeader := true)(
          cls := "expression-card",
          div(slot := "header", Icon()("function"), span(" Expression")),
          p("Type an AsciiMath expression:"),
          Textarea(
            _.label := "AsciiMath",
            _.placeholder := "e.g. x^2 * y + sin(x)",
            _.rows := 3
          )(
            value <-- asciiVar.signal,
            onInput.mapToValue --> asciiVar.writer
          ),
          div(
            cls := "rendered-math",
            child <-- asciiResult.map(
              _.map(mathlify.LaminarRenderer.render)
                .getOrElse(Callout(_.variant := "danger")("Invalid expression"): HtmlElement)
            )
          )
        ),
        // Variables
        Card(_.withHeader := true)(
          cls := "variables-card",
          div(slot := "header", Icon()("sliders"), span(" Variables")),
          p("Set values for each variable:"),
          div(
            cls := "variable-inputs",
            children <-- asciiResult.map { s =>
              s.fold(
                _ => Seq(Callout(_.variant := "warning")("Parse error"): HtmlElement),
                expr =>
                  val vars = mathlify.Evaluator.unboundVars(expr).toSeq.sorted
                  if vars.isEmpty then Seq(Callout(_.variant := "success")("No free variables — expression is fully evaluable!"): HtmlElement)
                  else
                    vars.map { v =>
                      Input(_.label := s"$v", _.placeholder := "0", _.tpe := "number")(
                        onInput.mapToValue --> { value =>
                          varMap.update(m => m + (v -> mathlify.Evaluator.parseConstant(value).getOrElse(Double.NaN)))
                        }
                      ): HtmlElement
                    }
                  end if
              )
            }
          )
        ),
        // Results — all partial derivatives at once
        Card(_.withHeader := true)(
          cls := "eval-card",
          div(slot := "header", Icon()("equals"), span(" Result")),
          p("The function value and all partial derivatives (computed in a single pass):"),
          div(
            cls := "eval-result",
            child <-- asciiResult
              .combineWith(varMap.signal)
              .map { case (parseResult, vars) =>
                parseResult.fold(
                  _ => span("Parse error"): HtmlElement,
                  expr =>
                    val freeVars = mathlify.Evaluator.unboundVars(expr)
                    val allBound = freeVars.forall(v => vars.contains(v) && !vars(v).isNaN)
                    if freeVars.isEmpty then
                      // No free variables — just evaluate
                      mathlify.Evaluator.eval(expr) match
                        case mathlify.Numeric(v) =>
                          Callout(_.variant := "success")(
                            div(cls := "result-row", span("f = "), strong(cls := "numeric-result", fmt(v)))
                          ): HtmlElement
                        case mathlify.EvalError(msg) =>
                          Callout(_.variant := "danger")(s"Error: $msg"): HtmlElement
                        case _ =>
                          Callout(_.variant := "neutral")("Partially reduced"): HtmlElement
                    else if !allBound then
                      Callout(_.variant := "neutral")(
                        "Set all variable values to compute the gradient."
                      ): HtmlElement
                    else
                      mathlify.ForwardDiff.gradient(expr, vars) match
                        case Right(dr) =>
                          Callout(_.variant := "success")(
                            cls := "derivative-result",
                            div(cls := "result-row", span("f = "), strong(cls := "numeric-result", fmt(dr.value))),
                            dr.partials.toSeq.sorted.map { case (name, deriv) =>
                              div(cls := "result-row", span(s"∂f/∂$name = "), strong(cls := "numeric-result", fmt(deriv)))
                            }
                          ): HtmlElement
                        case Left(msg) =>
                          Callout(_.variant := "danger")(s"Error: $msg"): HtmlElement
                    end if
                )
              }
          )
        )
      )
    )
  end evaluatorSection

  // ── Top-level render ──────────────────────────────────────────────────────

  def render(): HtmlElement =
    div(
      cls := "autodiff-page",
      theorySection(),
      Divider()(),
      examplesSection(),
      Divider()(),
      evaluatorSection()
    )
end AutoDiffPage
