package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*
import org.scalajs.dom

object AutoDiffPage:

  private def mathml(ascii: String): HtmlElement =
    mathlify.AsciiMath.translate(ascii) match
      case Right(expr) => mathlify.LaminarRenderer.renderInline(expr)
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
      h4("Transcendental & Trigonometric Functions"),
      p(
        "The chain rule extends naturally to unary functions. Applying a differentiable function ",
        mathml("f"),
        " to a dual number gives ",
        mathml("f(a, a') = (f(a), a' * f'(a))"),
        ":"
      ),
      div(
        cls := "dual-rules",
        dualRule("Exp", "exp(a, a') = (e^a, a' * e^a)"),
        dualRule("Natural Log", "ln(a, a') = (ln(a), (a')/(a))"),
        dualRule("Sine", "sin(a, a') = (sin(a), a' * cos(a))"),
        dualRule("Cosine", "cos(a, a') = (cos(a), -a' * sin(a))"),
        dualRule("Square Root", "sqrt(a, a') = (sqrt(a), (a') / (2 * sqrt(a)))")
      ),
      p(
        "Arithmetic rules follow from expanding with ",
        mathml("epsilon^2 = 0"),
        ". Transcendental rules follow from truncating their Taylor series at first order — both derivations are ",
        a(
          href := "#",
          onClick.preventDefault --> { _ =>
            dom.document.getElementById("deriving-rules").scrollIntoView()
          },
          "shown below."
        )
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

  // ── Deriving the rules ───────────────────────────────────────────────────

  private def derivingRulesSection(): HtmlElement =
    div(
      idAttr := "deriving-rules",
      cls := "deriving-rules-section",
      h2("Deriving the Dual Number Arithmetic Rules"),
      p(
        "Every dual number rule can be derived by plain algebra, using a single key fact: ",
        "we introduce a special symbol ",
        mathml("epsilon"),
        " (epsilon) that is ",
        em("nilpotent"),
        " — meaning ",
        mathml("epsilon^2 = 0"),
        ", yet ",
        mathml("epsilon != 0"),
        ". Think of it as an infinitesimally small perturbation."
      ),
      p(
        "A dual number is then written ",
        mathml("a + a' epsilon"),
        ", pairing a real value ",
        mathml("a"),
        " with a derivative part ",
        mathml("a'"),
        "."
      ),
      h3("Deriving the Multiplication Rule"),
      p(
        "Let's work through ",
        mathml("(a + a' epsilon)(b + b' epsilon)"),
        " step by step:"
      ),
      div(
        cls := "proof-steps",
        proofStep(
          "Expand",
          "(a + a' epsilon)(b + b' epsilon) = a*b + a*b' epsilon + a'*b epsilon + a'*b' epsilon^2",
          "Distribute like ordinary algebra"
        ),
        proofStep(
          "Apply ε² = 0",
          "= a*b + a*b' epsilon + a'*b epsilon + 0",
          "The ε² term vanishes — this is the whole trick"
        ),
        proofStep(
          "Collect ε terms",
          "= a*b + (a'*b + a*b') epsilon",
          "Factor out ε from the derivative part"
        ),
        proofStep(
          "Read off the rule",
          "(a, a') * (b, b') = (a*b, \\ a'*b + a*b')",
          "Value part: ab. Derivative part: a'b + ab' — exactly the product rule!"
        )
      ),
      h3("The Same Trick Works for Every Rule"),
      p("Addition needs no special treatment — it falls straight out of expanding:"),
      div(
        cls := "proof-steps",
        proofStep(
          "Add two dual numbers",
          "(a + a' epsilon) + (b + b' epsilon) = (a + b) + (a' + b') epsilon",
          "Grouping real and ε parts gives the addition rule directly"
        )
      ),
      p(
        "Division is a little more work. Write ",
        mathml("(a + a' epsilon) / (b + b' epsilon)"),
        " and multiply top and bottom by the 'conjugate' ",
        mathml("b - b' epsilon"),
        ":"
      ),
      div(
        cls := "proof-steps",
        proofStep(
          "Multiply by conjugate",
          "((a + a' epsilon)(b - b' epsilon)) / ((b + b' epsilon)(b - b' epsilon))",
          "Standard rationalisation trick"
        ),
        proofStep(
          "Denominator",
          "(b + b' epsilon)(b - b' epsilon) = b^2 - (b')^2 epsilon^2 = b^2",
          "ε² = 0, so the cross term vanishes"
        ),
        proofStep(
          "Numerator",
          "(a + a' epsilon)(b - b' epsilon) = a*b + (a'*b - a*b') epsilon",
          "Same expansion as before, ε² term dropped"
        ),
        proofStep(
          "Combine",
          "(a, a') / (b, b') = (a/b, \\ (a'*b - a*b') / b^2)",
          "Exactly the quotient rule — no calculus needed!"
        )
      ),
      h3("Transcendental Functions via Taylor Expansion"),
      p(
        "Algebraic rules handle +, −, ×, ÷, but what about exp, sin, cos, ln? The same ",
        mathml("epsilon^2 = 0"),
        " property works here too, via Taylor series. ",
        "Any analytic function ",
        mathml("f"),
        " evaluated at ",
        mathml("a + a' epsilon"),
        " has all quadratic and higher epsilon terms vanish, leaving exactly:"
      ),
      div(
        cls := "rendered-math highlight-formula",
        mathml("f(a + a' epsilon) = f(a) + a' f'(a) epsilon")
      ),
      h4("Worked Example: Exp"),
      p(
        "Let's trace ",
        mathml("e^(a + a' epsilon)"),
        " step by step using the Taylor series ",
        mathml("e^x = 1 + x + x^2/(2!) + x^3/(3!) + ..."),
        ":"
      ),
      div(
        cls := "proof-steps",
        proofStep(
          "Substitute the dual number",
          "e^(a + a' epsilon) = 1 + (a + a' epsilon) + (a + a' epsilon)^2/(2!) + (a + a' epsilon)^3/(3!) + ...",
          "Replace x with the dual number a + a'ε"
        ),
        proofStep(
          "Expand the first few terms explicitly, dropping ε² = 0 terms",
          "= 1 + (a + a' epsilon) + (a^2 + 2 a a' epsilon)/(2!) + (a^3 + 3 a^2 a' epsilon)/(3!) + ...",
          "Each term contributes both a real part and an ε part"
        ),
        proofStep(
          "Note: each power 'drops its higher than ε² terms' ",
          "(a + a' epsilon)^n = a^n + n * a^(n-1) * a' * epsilon",
          "Because ε² = 0. By the binomial theorem. Every term with ε² or higher vanishes, leaving only the real part and the linear ε part"
        ),
        proofStep(
          "Separate real and ε columns",
          "= (1 + a + a^2/(2!) + a^3/(3!) + ...) + (a' + (2 a a')/(2!) + (3 a^2 a')/(3!) + ...) * epsilon",
          "Group all the real parts together and all the ε coefficients together"
        ),
        proofStep(
          "Recognise both series as e^a",
          "= e^a + a' * e^a * epsilon",
          "Both bracketed sums are the Taylor series for eᵃ"
        ),
        proofStep(
          "Result",
          "exp(a, a') = (e^a, a' * e^a)",
          "The chain rule d/dx[eˣ] = eˣ emerges automatically — no calculus rules needed, just algebra!"
        )
      ),
      p(
        "The same reasoning applies to sin, cos, ln and any other analytic function: ",
        "substitute the dual number, drop all ",
        mathml("epsilon^2"),
        " terms, and read off the derivative from the ",
        mathml("epsilon"),
        " coefficient."
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
    val varMap = Var(Map("x" -> 2.0, "y" -> 3.0))

    div(
      cls := "evaluator-section",
      h2("Interactive Derivative Evaluator"),
      p("Enter an expression, set variable values, and see partial derivatives computed in a single pass."),
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
            // Prune stale entries from varMap whenever the expression changes
            asciiResult.changes.map {
              case Right(expr) => mathlify.Evaluator.unboundVars(expr)
              case Left(_)     => Set.empty[String]
            } --> { currentVars =>
              varMap.update(m => m.filter((k, _) => currentVars.contains(k)))
            },
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
                        // Bind displayed value back to varMap so re-rendered inputs show stored values
                        value <-- varMap.signal.map(m => m.get(v).map(fmt).getOrElse("")),
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
                    val activeVars = vars.filter((k, _) => freeVars.contains(k))
                    val allBound = freeVars.forall(v => activeVars.contains(v) && !activeVars(v).isNaN)
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
                      mathlify.ForwardDiff.gradient(expr, activeVars) match
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

  // ── Complexity section ──────────────────────────────────────────────────

  private def complexitySection(): HtmlElement =
    div(
      cls := "complexity-section",
      h2("Complexity & Limitations"),
      h3("Forward-Mode is O(N) in the Number of Inputs"),
      p(
        "To compute the full gradient of a function with ",
        mathml("N"),
        " input variables, forward-mode AD must be run ",
        mathml("N"),
        " separate times — once per variable, each time seeding that variable's derivative part to 1 and all others to 0. ",
        "Each pass produces one partial derivative, so computing all ",
        mathml("N"),
        " partial derivatives costs ",
        mathml("O(N)"),
        " evaluations of the full function."
      ),
      p(
        "You can see this in the interactive evaluator above: it calls ",
        code("ForwardDiff.gradient"),
        ", which internally runs one forward pass per variable. For two variables that is two passes; for ten it is ten."
      ),
      h3("Why This Matters at Scale"),
      p(
        "For a function with a ",
        em("small"),
        " number of inputs — like the examples on this page — forward mode is efficient. ",
        "However, consider a neural network with ",
        mathml("N = 10^8"),
        " parameters. Training requires the gradient of the scalar loss with respect to every weight. ",
        "Forward-mode would need ",
        mathml("10^8"),
        " full forward passes through the entire network — completely intractable."
      ),
      p(
        "The relationship is:"
      ),
      div(
        cls := "rendered-math highlight-formula",
        mathml("text(cost) = N_{text(inputs)} xx text(cost of one forward pass)")
      ),
      p(
        "Forward-mode is therefore well-suited to functions with ",
        strong("few inputs and many outputs"),
        " (e.g. computing a Jacobian row-by-row). It is poorly suited to functions with ",
        strong("many inputs and few outputs"),
        " — exactly the shape of a loss function in machine learning."
      ),
      h3("The Alternative: Reverse-Mode AD (Backpropagation)"),
      p(
        "Reverse-mode AD — also known as backpropagation in the ML world — computes the full gradient in a single pass. ",
        "It operates in ",
        mathml("O(M)"),
        " where ",
        mathml("M"),
        " is the number of ",
        em("outputs"),
        ", regardless of how many inputs there are. ",
        "For a scalar loss function (",
        mathml("M = 1"),
        "), one reverse pass gives all ",
        mathml("N"),
        " gradients at once — making it the foundation of every modern deep learning framework."
      ),
      p(
        "The trade-off is that reverse-mode requires storing intermediate values during the forward pass ",
        "(the 'tape'), carrying a memory cost proportional to the depth of the computation graph. ",
        "Forward-mode needs no tape and has minimal memory overhead — which can make it preferable ",
        "when differentiating through long sequences or in memory-constrained environments."
      ),
      h3("Forward Mode vs Backward Mode"),
      div(
        cls := "mode-comparison",
        h4("Forward Mode"),
        Callout(_.variant := "success")(
          div(slot := "header", strong("Forward-mode strengths")),
          ul(
            li(
              strong("Conceptual simplicity — "),
              "the dual number model is entirely self-contained. Every value carries its own derivative; ",
              "there is nothing else to understand."
            ),
            li(
              strong("Stateless & tape-free — "),
              "each operation is a pure function of its dual-number inputs. No intermediate values need ",
              "to be stored, no computation graph built, no backward pass scheduled."
            ),
            li(
              strong("Natural functional composition — "),
              "because each dual number is self-describing, functions compose without any special wiring: ",
              mathml("f(g(x))"),
              " just works. The chain rule falls out automatically from the arithmetic."
            ),
            li(
              strong("Preferred when inputs ≪ outputs — "),
              "computing a full Jacobian row-by-row, differentiating through recurrent sequences, ",
              "or working in memory-constrained settings."
            )
          )
        ),
        p(),
        h4("Backward Mode"),
        Callout(_.variant := "neutral")(
          div(slot := "header", strong("Reverse-mode strengths")),
          ul(
            li(
              strong("O(M) in outputs — "),
              "one backward pass gives all ",
              mathml("N"),
              " gradients when there is a single scalar output (",
              mathml("M = 1"),
              "), making it indispensable for training neural networks."
            ),
            li(
              strong("Preferred when inputs ≫ outputs — "),
              "the standard choice for any loss-function gradient in machine learning."
            ),
            li(
              strong("Trade-off: tape memory — "),
              "intermediate activations must be stored during the forward pass, with memory cost proportional to the depth of the computation graph."
            )
          )
        )
      )
    )

  // ── Top-level render ──────────────────────────────────────────────────────

  def render(): HtmlElement =
    div(
      cls := "autodiff-page",
      evaluatorSection(),
      Divider()(),
      theorySection(),
      Divider()(),
      examplesSection(),
      Divider()(),
      derivingRulesSection(),
      Divider()(),
      complexitySection()
    )
end AutoDiffPage
