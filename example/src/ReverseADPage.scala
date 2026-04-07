package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*
import org.scalajs.dom

object ReverseADPage:

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
      h2("What is Reverse-Mode Automatic Differentiation?"),
      p(
        "Reverse-mode AD (also known as ",
        strong("backpropagation"),
        ") computes all partial derivatives in a single backward sweep through the computation graph. ",
        "While forward-mode propagates derivatives ",
        em("forward"),
        " from inputs to outputs, reverse-mode propagates ",
        em("adjoints"),
        " (sensitivities) ",
        strong("backward"),
        " from the output to all inputs."
      ),
      h3("The Two-Phase Algorithm"),
      p("Reverse-mode AD works in two phases:"),
      div(
        cls := "dual-rules",
        dualRule(
          "1. Forward Pass (Build Tape)",
          "f(x) rarr v_0, v_1, ..., v_n"
        ),
        dualRule(
          "2. Backward Pass (Propagate Adjoints)",
          "bar(v_n) = 1, bar(v_i) += bar(v_j) * (del v_j)/(del v_i)"
        )
      ),
      h3("Adjoint Rules"),
      p(
        "Each node in the graph accumulates an ",
        strong("adjoint"),
        " — the derivative of the final output with respect to that node's value. ",
        "The output node starts with adjoint = 1. Each operation pushes adjoints to its parents:"
      ),
      div(
        cls := "dual-rules",
        dualRule("Addition: f = a + b", "bar(a) += bar(f), bar(b) += bar(f)"),
        dualRule("Subtraction: f = a − b", "bar(a) += bar(f), bar(b) += -bar(f)"),
        dualRule("Multiplication: f = a × b", "bar(a) += bar(f) * b, bar(b) += bar(f) * a"),
        dualRule("Division: f = a ÷ b", "bar(a) += bar(f) / b, bar(b) += -bar(f) * a / b^2"),
        dualRule("Power: f = a^b", "bar(a) += bar(f) * b * a^(b-1)"),
        dualRule("Chain rule (general)", "bar(x) += bar(f) * (df)/(dx)")
      ),
      h3("Why Reverse Mode?"),
      p(
        "For a function with ",
        mathml("N"),
        " inputs and ",
        mathml("M"),
        " outputs, forward-mode costs ",
        mathml("O(N)"),
        " passes but reverse-mode costs only ",
        mathml("O(M)"),
        " passes. ",
        "For a scalar loss function (",
        mathml("M = 1"),
        "), ",
        strong("one reverse pass gives all N gradients at once"),
        " — the foundation of modern deep learning."
      ),
      Divider()(),
      p(
        "The trade-off: reverse-mode must store intermediate values from the forward pass (the 'tape'), ",
        "using memory proportional to the depth of the computation graph. Forward-mode needs no tape."
      )
    )

  private def dualRule(name: String, ascii: String): HtmlElement =
    div(
      cls := "dual-rule",
      strong(name, ": "),
      div(cls := "rendered-math", mathml(ascii))
    )

  // ── SVG computation graph ─────────────────────────────────────────────────

  private val SVG_W = 700
  private val SVG_H_BASE = 200
  private val NODE_RX = 36.0
  private val NODE_RY = 22.0
  private val LAYER_GAP_X = 120.0

  private case class NodeLayout(x: Double, y: Double, idx: Int)

  /** Lay out the tape nodes in a left-to-right graph. Nodes at the same depth level are stacked vertically. */
  private def layoutNodes(tape: Vector[mathlify.ReverseDiff.TapeNode]): (Vector[NodeLayout], Double) =
    if tape.isEmpty then return (Vector.empty, SVG_H_BASE.toDouble)

    // Compute depth of each node (max depth of parents + 1)
    val depths = Array.fill(tape.length)(0)
    for i <- tape.indices do
      if tape(i).parents.nonEmpty then
        depths(i) = tape(i).parents.map(depths(_)).max + 1

    val maxDepth = depths.max

    // Group nodes by depth
    val byDepth = tape.indices.groupBy(depths(_)).toSeq.sortBy(_._1)

    val maxNodesInLayer = byDepth.map(_._2.size).maxOption.getOrElse(1)
    val layerHeight = math.max(60.0 * maxNodesInLayer, SVG_H_BASE.toDouble)
    val svgH = layerHeight + 40.0

    val layouts = Array.ofDim[NodeLayout](tape.length)
    for (depth, indices) <- byDepth do
      val x = 60.0 + depth * LAYER_GAP_X
      val n = indices.size
      val startY = (svgH - (n - 1) * 60.0) / 2.0
      for (idx, rank) <- indices.sorted.zipWithIndex do layouts(idx) = NodeLayout(x, startY + rank * 60.0, idx)

    (layouts.toVector, svgH)
  end layoutNodes

  /** Build the SVG computation graph with clickable nodes. */
  private def buildGraph(
      tape: Vector[mathlify.ReverseDiff.TapeNode],
      adjoints: Array[Double],
      selectedIdx: Int,
      backStepIdx: Int,
      backSteps: List[mathlify.ReverseDiff.BackStep]
  ): HtmlElement =
    import com.raquo.laminar.api.L.svg as S

    if tape.isEmpty then return div(cls := "graph-empty", p("No computation graph to display."))

    val (layouts, svgH) = layoutNodes(tape)
    val svgW = layouts.map(_.x).max + 80.0

    // Which nodes/edges are highlighted by the current back step?
    val activeSteps = backSteps.take(backStepIdx)
    val currentStep = if backStepIdx > 0 && backStepIdx <= backSteps.length then Some(backSteps(backStepIdx - 1)) else None

    // Compute current adjoints at this step
    val currentAdjoints = Array.fill(tape.length)(0.0)
    if backStepIdx > 0 then
      // Replay steps up to backStepIdx
      currentAdjoints(tape.length - 1) = 1.0
      for step <- activeSteps do currentAdjoints(step.nodeIndex) = step.adjointBefore + step.adjointIncrement

    // Draw edges
    val edgeElems = tape.indices.flatMap { i =>
      tape(i).parents.map { pi =>
        val src = layouts(pi)
        val dst = layouts(i)
        // Is this edge part of the current step?
        val isActive = currentStep.exists(s => s.fromNode == i && s.nodeIndex == pi)
        val isVisited = activeSteps.exists(s => s.fromNode == i && s.nodeIndex == pi)
        val color =
          if isActive then "#ef4444"
          else if isVisited then "#93c5fd"
          else "#cbd5e1"
        val width = if isActive then "2.5" else "1.5"

        // Arrow from src to dst (parent to child = left to right)
        val (lineD, headD) = arrowGeom(src.x, src.y, dst.x, dst.y)
        List(
          S.path(S.d := lineD, S.style := s"fill: none; stroke: $color; stroke-width: ${width}px;"),
          S.path(S.d := headD, S.style := s"fill: $color; stroke: none;")
        )
      }
    }.flatten

    // Draw nodes
    val nodeElems = tape.indices.flatMap { i =>
      val n = tape(i)
      val l = layouts(i)
      val isOutput = i == tape.length - 1
      val isSelected = i == selectedIdx
      val isCurrentTarget = currentStep.exists(_.nodeIndex == i)
      val isCurrentSource = currentStep.exists(_.fromNode == i)

      val hasAdjoint = backStepIdx > 0 && currentAdjoints(i) != 0.0

      val fill =
        if isCurrentTarget then "#fde68a" // yellow highlight
        else if isCurrentSource then "#fca5a5" // red highlight
        else if isSelected then "#bfdbfe" // blue selected
        else if isOutput then "#bbf7d0" // green output
        else if hasAdjoint then "#e0e7ff" // light purple for nodes with adjoints
        else "#f8fafc"

      val stroke =
        if isSelected || isCurrentTarget || isCurrentSource then "#1e40af"
        else if isOutput then "#16a34a"
        else "#94a3b8"

      val strokeW = if isSelected || isCurrentTarget || isCurrentSource then "2.5" else "1.5"

      val nodeLabel = n.op match
        case mathlify.ReverseDiff.Op.Const    => n.label
        case v: mathlify.ReverseDiff.Op.Var   => v.name
        case mathlify.ReverseDiff.Op.AddOp    => "+"
        case mathlify.ReverseDiff.Op.SubOp    => "−"
        case mathlify.ReverseDiff.Op.MulOp    => "×"
        case mathlify.ReverseDiff.Op.DivOp    => "÷"
        case mathlify.ReverseDiff.Op.PowOp    => "^"
        case mathlify.ReverseDiff.Op.NegOp    => "−()"
        case mathlify.ReverseDiff.Op.SinOp    => "sin"
        case mathlify.ReverseDiff.Op.CosOp    => "cos"
        case mathlify.ReverseDiff.Op.TanOp    => "tan"
        case mathlify.ReverseDiff.Op.ExpOp    => "exp"
        case mathlify.ReverseDiff.Op.LogOp    => "log"
        case mathlify.ReverseDiff.Op.SqrtOp   => "√"

      val adjText =
        if backStepIdx > 0 then s"adj=${fmt(currentAdjoints(i))}"
        else ""

      List(
        S.rect(
          S.x := fmtD(l.x - NODE_RX),
          S.y := fmtD(l.y - NODE_RY),
          S.width := fmtD(NODE_RX * 2),
          S.height := fmtD(NODE_RY * 2),
          S.rx := "6",
          S.ry := "6",
          S.style := s"fill: $fill; stroke: $stroke; stroke-width: ${strokeW}px; cursor: pointer;"
        ),
        S.text(
          S.x := fmtD(l.x),
          S.y := fmtD(l.y - 5),
          S.style := "text-anchor: middle; dominant-baseline: central; font-size: 12px; font-weight: bold; fill: #1e293b; pointer-events: none;",
          TextNode(nodeLabel)
        ),
        S.text(
          S.x := fmtD(l.x),
          S.y := fmtD(l.y + 9),
          S.style := "text-anchor: middle; dominant-baseline: central; font-size: 9px; fill: #64748b; pointer-events: none;",
          TextNode(s"=${fmt(n.value)}")
        ),
        // Adjoint label below node (only during backward walk)
        if adjText.nonEmpty then
          S.text(
            S.x := fmtD(l.x),
            S.y := fmtD(l.y + NODE_RY + 12),
            S.style := s"text-anchor: middle; font-size: 9px; fill: ${if isCurrentTarget then "#dc2626" else "#6366f1"}; font-weight: bold; pointer-events: none;",
            TextNode(adjText)
          )
        else S.g() // empty
      )
    }

    val svgElem = S.svg(
      S.viewBox := s"0 0 ${fmtD(svgW)} ${fmtD(svgH)}",
      S.style := s"width: 100%; max-width: ${svgW.toInt}px; height: auto; display: block;",
      edgeElems,
      nodeElems
    )

    div(cls := "graph-svg-container", svgElem)
  end buildGraph

  private def fmtD(d: Double): String = f"$d%.2f"

  private def arrowGeom(x1: Double, y1: Double, x2: Double, y2: Double): (String, String) =
    val dx = x2 - x1; val dy = y2 - y1
    val dist = math.sqrt(dx * dx + dy * dy)
    if dist < 0.01 then return ("", "")

    // Shorten by node radius
    val nx = dx / dist; val ny = dy / dist
    val sx = x1 + nx * (NODE_RX + 2)
    val sy = y1 + ny * (NODE_RY + 2)
    val ex = x2 - nx * (NODE_RX + 4)
    val ey = y2 - ny * (NODE_RY + 4)

    val lineD = s"M ${fmtD(sx)},${fmtD(sy)} L ${fmtD(ex)},${fmtD(ey)}"

    // Arrowhead
    val px = -ny; val py = nx
    val sz = 5.0
    val b1X = ex - nx * sz + px * sz * 0.5
    val b1Y = ey - ny * sz + py * sz * 0.5
    val b2X = ex - nx * sz - px * sz * 0.5
    val b2Y = ey - ny * sz - py * sz * 0.5
    val headD = s"M ${fmtD(b1X)},${fmtD(b1Y)} L ${fmtD(ex)},${fmtD(ey)} L ${fmtD(b2X)},${fmtD(b2Y)} Z"

    (lineD, headD)
  end arrowGeom

  // ── Interactive evaluator with graph ────────────────────────────────────

  private def evaluatorSection(): HtmlElement =
    val asciiVar = Var("x^2 * y + y^3")
    val asciiResult = asciiVar.signal.map(mathlify.AsciiMath.translate)
    val varMap = Var(Map("x" -> 2.0, "y" -> 3.0))
    val backStepVar = Var(0) // which backward step we're viewing
    val selectedNodeVar = Var(-1) // clicked node index

    // Compute reverse-mode result reactively
    val reverseResult: Signal[Option[mathlify.ReverseDiff.ReverseResult]] =
      asciiResult.combineWith(varMap.signal).map { case (parseResult, vars) =>
        parseResult.toOption.flatMap { expr =>
          val freeVars = mathlify.Evaluator.unboundVars(expr)
          val activeVars = vars.filter((k, _) => freeVars.contains(k))
          val allBound = freeVars.forall(v => activeVars.contains(v) && !activeVars(v).isNaN)
          if allBound then mathlify.ReverseDiff.reverseGradient(expr, activeVars).toOption
          else None
        }
      }

    // Reset step when expression or variables change
    val resetObs = reverseResult.changes --> { _ =>
      backStepVar.set(0)
      selectedNodeVar.set(-1)
    }

    div(
      cls := "evaluator-section reverse-evaluator-section",
      resetObs,
      h2("Interactive Reverse-Mode Evaluator"),
      p(
        "Enter an expression, set variable values, and step through the backward pass. ",
        "Click nodes to inspect them, or use the step controls to walk through the adjoint propagation."
      ),
      div(
        cls := "cards-grid",
        // Expression input card
        Card(_.withHeader := true)(
          cls := "expression-card",
          div(slot := "header", Icon()("function"), span(" Expression")),
          p("Type an AsciiMath expression:"),
          Textarea(
            _.label := "AsciiMath",
            _.placeholder := "e.g. x^2 * y + sin(x)",
            _.rows := 2
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
        // Variables card
        Card(_.withHeader := true)(
          cls := "variables-card",
          div(slot := "header", Icon()("sliders"), span(" Variables")),
          p("Set values for each variable:"),
          div(
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
                  if vars.isEmpty then Seq(Callout(_.variant := "success")("No free variables"): HtmlElement)
                  else
                    vars.map { v =>
                      Input(_.label := s"$v", _.placeholder := "0", _.tpe := "number")(
                        value <-- varMap.signal.map(m => m.get(v).map(fmt).getOrElse("")),
                        onInput.mapToValue --> { value =>
                          varMap.update(m => m + (v -> mathlify.Evaluator.parseConstant(value).getOrElse(Double.NaN)))
                        }
                      ): HtmlElement
                    }
              )
            }
          )
        ),
        // Results card
        Card(_.withHeader := true)(
          cls := "eval-card reverse-result-card",
          div(slot := "header", Icon()("equals"), span(" Gradients")),
          p("All partial derivatives computed in a single backward pass:"),
          div(
            cls := "eval-result",
            child <-- reverseResult.map {
              case Some(rr) =>
                Callout(_.variant := "success")(
                  cls := "derivative-result",
                  div(cls := "result-row", span("f = "), strong(cls := "numeric-result", fmt(rr.value))),
                  rr.partials.toSeq.sorted.map { case (name, deriv) =>
                    div(cls := "result-row", span(s"∂f/∂$name = "), strong(cls := "numeric-result", fmt(deriv)))
                  }
                ): HtmlElement
              case None =>
                Callout(_.variant := "neutral")(
                  "Set all variable values to compute the gradient."
                ): HtmlElement
            }
          )
        )
      ),
      // ── Computation graph section ──
      div(
        cls := "graph-section",
        h3("Computation Graph"),
        p(
          "The graph below shows the computation tape built during the forward pass. ",
          "Use the step controls to walk through the backward pass and see how adjoints flow from the output to the inputs."
        ),
        // Step controls
        child <-- reverseResult.map {
          case Some(rr) =>
            val totalSteps = rr.backSteps.length
            div(
              cls := "step-controls",
              Button(_.size := "small")(
                "⏮ Reset",
                onClick --> { _ => backStepVar.set(0) }
              ),
              Button(_.size := "small")(
                "← Prev",
                disabled <-- backStepVar.signal.map(_ <= 0),
                onClick --> { _ => backStepVar.update(s => math.max(0, s - 1)) }
              ),
              span(
                cls := "step-counter",
                child.text <-- backStepVar.signal.map(s => s"Step $s / $totalSteps")
              ),
              Button(_.size := "small")(
                "Next →",
                disabled <-- backStepVar.signal.map(_ >= totalSteps),
                onClick --> { _ => backStepVar.update(s => math.min(totalSteps, s + 1)) }
              ),
              Button(_.size := "small")(
                "⏭ All",
                onClick --> { _ => backStepVar.set(totalSteps) }
              )
            ): HtmlElement
          case None => div(): HtmlElement
        },
        // Step description
        child <-- reverseResult
          .combineWith(backStepVar.signal)
          .map {
            case (Some(rr), step) if step > 0 && step <= rr.backSteps.length =>
              val bs = rr.backSteps(step - 1)
              val fromLabel = rr.tape(bs.fromNode).label
              val toLabel = rr.tape(bs.nodeIndex).label
              Callout(_.variant := "brand")(
                cls := "step-description",
                div(
                  strong(s"Step $step: "),
                  span(s"From node "),
                  code(s"$fromLabel (=${fmt(rr.tape(bs.fromNode).value)})"),
                  span(s" → "),
                  code(s"$toLabel (=${fmt(rr.tape(bs.nodeIndex).value)})"),
                ),
                div(
                  span("Rule: "),
                  strong(bs.rule)
                ),
                div(
                  span(s"Adjoint of "),
                  code(toLabel),
                  span(s": ${fmt(bs.adjointBefore)} + ${fmt(bs.adjointIncrement)} = ${fmt(bs.adjointBefore + bs.adjointIncrement)}")
                )
              ): HtmlElement
            case (Some(_), 0) =>
              Callout(_.variant := "neutral")(
                cls := "step-description",
                "Click ", strong("Next →"), " to start the backward pass. The output node begins with adjoint = 1."
              ): HtmlElement
            case _ => div(): HtmlElement
          },
        // SVG graph
        div(
          cls := "graph-container",
          child <-- reverseResult
            .combineWith(backStepVar.signal, selectedNodeVar.signal)
            .map { case (rrOpt, step, selected) =>
              rrOpt match
                case Some(rr) =>
                  buildGraph(rr.tape, rr.adjoints, selected, step, rr.backSteps): HtmlElement
                case None =>
                  Callout(_.variant := "neutral")("Enter an expression and set variables to see the computation graph."): HtmlElement
            }
        ),
        // Tape table
        child <-- reverseResult.combineWith(backStepVar.signal).map {
          case (Some(rr), step) => tapeTable(rr, step): HtmlElement
          case _                => div(): HtmlElement
        }
      )
    )
  end evaluatorSection

  /** Render the tape as a table showing forward values and current adjoints. */
  private def tapeTable(rr: mathlify.ReverseDiff.ReverseResult, backStepIdx: Int): HtmlElement =
    // Replay adjoints up to step
    val currentAdjoints = Array.fill(rr.tape.length)(0.0)
    if backStepIdx > 0 then
      currentAdjoints(rr.tape.length - 1) = 1.0
      for step <- rr.backSteps.take(backStepIdx) do
        currentAdjoints(step.nodeIndex) = step.adjointBefore + step.adjointIncrement

    div(
      cls := "tape-table-container",
      h4("Computation Tape"),
      table(
        cls := "tape-table",
        thead(
          tr(
            th("#"),
            th("Operation"),
            th("Label"),
            th("Value"),
            th("Parents"),
            th("Adjoint (∂out/∂node)")
          )
        ),
        tbody(
          rr.tape.zipWithIndex.map { case (node, i) =>
            val opName = node.op match
              case mathlify.ReverseDiff.Op.Const        => "const"
              case v: mathlify.ReverseDiff.Op.Var       => s"var(${v.name})"
              case mathlify.ReverseDiff.Op.AddOp        => "add"
              case mathlify.ReverseDiff.Op.SubOp        => "sub"
              case mathlify.ReverseDiff.Op.MulOp        => "mul"
              case mathlify.ReverseDiff.Op.DivOp        => "div"
              case mathlify.ReverseDiff.Op.PowOp        => "pow"
              case mathlify.ReverseDiff.Op.NegOp        => "neg"
              case mathlify.ReverseDiff.Op.SinOp        => "sin"
              case mathlify.ReverseDiff.Op.CosOp        => "cos"
              case mathlify.ReverseDiff.Op.TanOp        => "tan"
              case mathlify.ReverseDiff.Op.ExpOp        => "exp"
              case mathlify.ReverseDiff.Op.LogOp        => "log"
              case mathlify.ReverseDiff.Op.SqrtOp       => "sqrt"
            val adj = currentAdjoints(i)
            val adjStr = if backStepIdx == 0 && i != rr.tape.length - 1 then "—" else fmt(adj)
            val isOutput = i == rr.tape.length - 1
            tr(
              cls := (if isOutput then "tape-row-output" else ""),
              td(i.toString),
              td(code(opName)),
              td(node.label),
              td(fmt(node.value)),
              td(node.parents.mkString(", ")),
              td(cls := "adjoint-cell", adjStr)
            )
          }
        )
      )
    )
  end tapeTable

  // ── Comparison section ──────────────────────────────────────────────────

  private def comparisonSection(): HtmlElement =
    div(
      cls := "comparison-section",
      h2("Forward Mode vs Reverse Mode"),
      div(
        cls := "mode-comparison",
        h4("Forward Mode"),
        Callout(_.variant := "success")(
          div(slot := "header", strong("Forward-mode")),
          ul(
            li(
              strong("Cost: "),
              mathml("O(N)"),
              " — one pass per input variable."
            ),
            li(
              strong("Mechanism: "),
              "Dual numbers ",
              mathml("(v, v')"),
              " carry derivatives forward through the computation."
            ),
            li(
              strong("No tape: "),
              "Stateless, no memory overhead for intermediate values."
            ),
            li(
              strong("Best for: "),
              "Few inputs, many outputs (e.g. Jacobians)."
            )
          )
        ),
        p(),
        h4("Reverse Mode"),
        Callout(_.variant := "brand")(
          div(slot := "header", strong("Reverse-mode (this page)")),
          ul(
            li(
              strong("Cost: "),
              mathml("O(M)"),
              " — one backward pass per output. For scalar loss (",
              mathml("M=1"),
              "), all gradients in one pass."
            ),
            li(
              strong("Mechanism: "),
              "Build computation tape during forward pass, propagate adjoints backward."
            ),
            li(
              strong("Tape memory: "),
              "Must store all intermediate values — memory proportional to computation depth."
            ),
            li(
              strong("Best for: "),
              "Many inputs, few outputs (e.g. neural network training)."
            )
          )
        )
      )
    )

  // ── Top-level render ──────────────────────────────────────────────────────

  def render(): HtmlElement =
    div(
      cls := "autodiff-page reverse-ad-page",
      evaluatorSection(),
      Divider()(),
      theorySection(),
      Divider()(),
      comparisonSection()
    )
end ReverseADPage
