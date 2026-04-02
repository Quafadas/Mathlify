package mathlify.example

import com.raquo.laminar.api.L.{*, given}
import io.github.nguyenyou.webawesome.laminar.*

object ClockPage:

  // ── SVG layout constants ───────────────────────────────────────────────────
  private val SVG_W   = 280
  private val SVG_H   = 280
  private val CX      = SVG_W / 2.0
  private val CY      = SVG_H / 2.0
  private val CLOCK_R = 100.0 // ring radius (where node centres sit)
  private val LABEL_R = 120.0 // number label radius
  private val NODE_R  = 11.0  // node circle radius

  // ── Reactive state ─────────────────────────────────────────────────────────
  private val nVar    = Var(12)
  private val aVar    = Var(3)
  private val bVar    = Var(5)
  private val opVar   = Var("add")    // "add" | "mul" | "pow"
  private val modeVar = Var("single") // "single" | "pattern"

  // ── Modular arithmetic ─────────────────────────────────────────────────────
  private def modN(x: Int, n: Int): Int = ((x % n) + n) % n

  private def powMod(base: Int, exp: Int, n: Int): Int =
    if n <= 1 then 0
    else if exp <= 0 then 1 % n
    else
      var result = 1L
      var b      = base.toLong % n
      var e      = exp
      while e > 0 do
        if (e & 1) == 1 then result = (result * b) % n
        b = (b * b) % n
        e >>= 1
      result.toInt

  // Orbit of `start` under repeated application of the chosen step function.
  // Returns the sequence including the "return to start" node at the end.
  private def orbit(start: Int, b: Int, n: Int, op: String): List[Int] =
    val s = modN(start, n)
    def step(x: Int): Int = op match
      case "add" => modN(x + b, n)
      case "mul" => modN(x * b, n)
      case "pow" => modN(x * s, n) // repeatedly multiply by a (power sequence)
      case _     => x
    val buf = scala.collection.mutable.ListBuffer[Int]()
    buf += s
    var cur   = step(s)
    var limit = n + 1
    while cur != buf.head && limit > 0 do
      buf += cur
      cur = step(cur)
      limit -= 1
    buf += cur // show return-to-start
    buf.toList

  // All arrows x → f(x) for pattern mode
  private def patternArrows(a: Int, b: Int, n: Int, op: String): List[(Int, Int)] =
    (0 until n).map { x =>
      val y = op match
        case "add" => modN(x + b, n)
        case "mul" => modN(a * x, n)
        case "pow" => powMod(x, b, n)
        case _     => x
      (x, y)
    }.toList

  private def singleResult(a: Int, b: Int, n: Int, op: String): Int =
    op match
      case "add" => modN(a + b, n)
      case "mul" => modN(a * b, n)
      case "pow" => powMod(a, b, n)
      case _     => 0

  // ── SVG geometry ───────────────────────────────────────────────────────────
  private def nodeAngle(i: Int, n: Int): Double =
    2 * Math.PI * i / n - Math.PI / 2

  private def nodeXY(i: Int, n: Int, r: Double): (Double, Double) =
    val theta = nodeAngle(i, n)
    (CX + r * Math.cos(theta), CY + r * Math.sin(theta))

  private def fmt(d: Double): String = f"$d%.2f"

  // Arrow geometry: returns (bezier path string, arrowhead path string)
  private def arrowGeom(from: Int, to: Int, n: Int): (String, String) =
    val (x1, y1) = nodeXY(from, n, CLOCK_R)
    val (x2, y2) = nodeXY(to, n, CLOCK_R)
    if from == to then
      val linePath =
        s"M ${fmt(x1)},${fmt(y1)} A 12,12 0 1,1 ${fmt(x1 + 0.01)},${fmt(y1 + 0.01)}"
      (linePath, "")
    else
      val mx   = (x1 + x2) / 2; val my = (y1 + y2) / 2
      val dx   = CX - mx; val dy = CY - my
      val dist = Math.sqrt(dx * dx + dy * dy)
      val bend = if dist < 1.0 then 0.0 else Math.min(30.0 / dist, 0.5)
      val cpX  = mx + dx * bend; val cpY = my + dy * bend

      def shorten(px: Double, py: Double, qx: Double, qy: Double, d: Double): (Double, Double) =
        val ex  = px - qx; val ey = py - qy
        val len = Math.sqrt(ex * ex + ey * ey)
        if len < 0.01 then (px, py)
        else (px - ex / len * d, py - ey / len * d)

      val (sx, sy) = shorten(x1, y1, cpX, cpY, NODE_R + 1)
      val (ex, ey) = shorten(x2, y2, cpX, cpY, NODE_R + 3)
      val linePath = s"M ${fmt(sx)},${fmt(sy)} Q ${fmt(cpX)},${fmt(cpY)} ${fmt(ex)},${fmt(ey)}"

      // Arrowhead triangle pointing in the bezier tangent direction at the end
      val adx  = ex - cpX; val ady = ey - cpY
      val alen = Math.sqrt(adx * adx + ady * ady)
      val headPath =
        if alen < 0.1 then ""
        else
          val nx = adx / alen; val ny = ady / alen
          val px = -ny; val py = nx
          val sz = 5.5
          val b1X = ex - nx * sz + px * sz * 0.5; val b1Y = ey - ny * sz + py * sz * 0.5
          val b2X = ex - nx * sz - px * sz * 0.5; val b2Y = ey - ny * sz - py * sz * 0.5
          s"M ${fmt(b1X)},${fmt(b1Y)} L ${fmt(ex)},${fmt(ey)} L ${fmt(b2X)},${fmt(b2Y)} Z"

      (linePath, headPath)

  // ── SVG clock builder ──────────────────────────────────────────────────────
  private def buildSvg(
      n: Int,
      seq: List[Int],               // sequence (single mode) - last element is cycle return
      arrows: List[(Int, Int)],     // explicit arrows (pattern mode)
      mode: String
  ): HtmlElement =
    import com.raquo.laminar.api.L.svg as S

    val pathSet  = if mode == "single" then seq.dropRight(1).toSet else Set.empty[Int]
    val startSet = if mode == "single" then seq.headOption.toSet else Set.empty[Int]
    val arrowList = if mode == "pattern" then arrows else seq.zip(seq.tail)

    // Draw arrows (line + head)
    val arrowElems = arrowList.flatMap { case (from, to) =>
      val isReturn = mode == "single" && to == seq.head && from != seq.head
      val color    = if isReturn then "#ef4444" else "#2563eb"
      val (lineD, headD) = arrowGeom(from, to, n)
      val lineElem = S.path(
        S.d := lineD,
        S.style := s"fill: none; stroke: $color; stroke-width: 1.8px;"
      )
      val headElem =
        if headD.nonEmpty then
          List(S.path(S.d := headD, S.style := s"fill: $color; stroke: none;"))
        else List.empty
      lineElem :: headElem
    }

    // Node circles
    val nodeElems = (0 until n).map { i =>
      val (x, y)   = nodeXY(i, n, CLOCK_R)
      val inPath   = pathSet.contains(i)
      val isStart  = startSet.contains(i)
      val isTarget = mode == "pattern" && arrows.exists(_._2 == i)
      val fill =
        if isStart then "#22c55e"
        else if inPath then "#93c5fd"
        else if isTarget then "#fde68a"
        else "#f1f5f9"
      val strokeW = if isStart || inPath then "2" else "1.5"
      val stroke  = if isStart || inPath then "#1e293b" else "#94a3b8"
      S.circle(
        S.cx := fmt(x),
        S.cy := fmt(y),
        S.r := fmt(NODE_R),
        S.style := s"fill: $fill; stroke: $stroke; stroke-width: ${strokeW}px;"
      )
    }

    // Number labels
    val labelElems = (0 until n).map { i =>
      val (x, y) = nodeXY(i, n, LABEL_R)
      val sz     = if n > 15 then 8 else if n > 11 then 10 else 12
      val bold   = if pathSet.contains(i) || startSet.contains(i) then "bold" else "normal"
      S.text(
        S.x := fmt(x),
        S.y := fmt(y),
        S.style :=
          s"text-anchor: middle; dominant-baseline: central; font-size: ${sz}px; fill: #1e293b; font-weight: $bold;",
        TextNode(i.toString)
      )
    }

    val svgElem = S.svg(
      S.viewBox := s"0 0 $SVG_W $SVG_H",
      S.style := s"width: ${SVG_W}px; height: ${SVG_H}px; display: block;",
      // Faint ring
      S.circle(
        S.cx := fmt(CX),
        S.cy := fmt(CY),
        S.r := fmt(CLOCK_R),
        S.style := "fill: none; stroke: #e2e8f0; stroke-width: 1px; stroke-dasharray: 4,3;"
      ),
      arrowElems,
      nodeElems,
      labelElems
    )
    // Wrap in a div so we return HtmlElement
    div(cls := "clock-svg-inner", svgElem)

  // ── Label helpers ──────────────────────────────────────────────────────────
  private def opLabel(op: String): String = op match
    case "add" => "Addition"
    case "mul" => "Multiplication"
    case "pow" => "Powers"
    case _     => op

  private def aLabel(op: String): String = op match
    case "pow" => "Base (a)"
    case _     => "Start value (a)"

  private def bLabel(op: String): String = op match
    case "add" => "Step (b)"
    case "mul" => "Multiplier (b)"
    case "pow" => "Exponent (k)"
    case _     => "b"

  private def exprLabel(a: Int, b: Int, n: Int, op: String): String = op match
    case "add" => s"$a + $b mod $n"
    case "mul" => s"$a × $b mod $n"
    case "pow" => s"$a^$b mod $n"
    case _     => ""

  private def patternDesc(a: Int, b: Int, n: Int, op: String): String = op match
    case "add" => s"x → x + $b  (mod $n)"
    case "mul" => s"x → $a · x  (mod $n)"
    case "pow" => s"x → x^$b  (mod $n)"
    case _     => ""

  // ── Page render ────────────────────────────────────────────────────────────
  def render(): HtmlElement =

    // Recompute clock SVG whenever relevant state changes
    val svgSignal =
      nVar.signal
        .combineWith(aVar.signal, bVar.signal, opVar.signal, modeVar.signal)
        .map { case (n, a, b, op, mode) =>
          val a2 = modN(a, n)
          if mode == "pattern" then
            buildSvg(n, List.empty, patternArrows(a2, b, n, op), mode)
          else
            buildSvg(n, orbit(a2, b, n, op), List.empty, mode)
        }

    val seqSignal =
      nVar.signal.combineWith(aVar.signal, bVar.signal, opVar.signal).map { case (n, a, b, op) =>
        orbit(modN(a, n), b, n, op)
      }

    div(
      cls := "clock-page",
      h2("Clock Arithmetic"),
      p(
        cls := "clock-intro",
        "Numbers can wrap around — just like hours on a clock. That wrapping is ",
        strong("modular arithmetic"),
        ". Experiment below to discover cycles, patterns and star polygons hidden in everyday maths."
      ),
      div(
        cls := "clock-main-layout",
        // ── Left column: clock SVG ─────────────────────────────────────────
        div(
          cls := "clock-visual-panel",
          // Modulus slider
          div(
            cls := "clock-modulus-row",
            label(cls := "clock-label", "Clock size:  "),
            input(
              typ := "range",
              cls := "clock-slider",
              minAttr := "2",
              maxAttr := "20",
              value <-- nVar.signal.map(_.toString),
              onInput.mapToValue --> { v => nVar.set(v.toIntOption.getOrElse(12)) }
            ),
            child <-- nVar.signal.map(n => span(cls := "clock-n-badge", s"n = $n"))
          ),
          // SVG clock
          div(cls := "clock-svg-container", child <-- svgSignal),
          // Colour legend
          div(
            cls := "clock-legend",
            span(cls := "legend-dot legend-start"),
            span(cls := "legend-text", "Start"),
            span(cls := "legend-dot legend-path"),
            span(cls := "legend-text", "Path"),
            span(cls := "legend-dot legend-return"),
            span(cls := "legend-text", "Return")
          )
        ),

        // ── Right column: controls ─────────────────────────────────────────
        div(
          cls := "clock-controls-panel",
          // Operation selector
          div(
            cls := "clock-op-selector",
            p(cls := "clock-label", "Operation"),
            div(
              cls := "clock-op-buttons",
              List("add", "mul", "pow").map { op =>
                button(
                  cls <-- opVar.signal.map(o =>
                    "clock-op-btn" + (if o == op then " clock-op-active" else "")
                  ),
                  opLabel(op),
                  onClick --> (_ => opVar.set(op))
                )
              }
            )
          ),

          // a input
          div(
            cls := "clock-input-row",
            label(
              cls := "clock-label",
              child <-- opVar.signal.map(op => span(aLabel(op) + ":"))
            ),
            input(
              typ := "number",
              cls := "clock-number-input",
              value <-- aVar.signal.map(_.toString),
              onInput.mapToValue --> { v => aVar.set(v.toIntOption.getOrElse(0)) }
            )
          ),

          // b input (hidden in pow-sequence display but still usable)
          div(
            cls := "clock-input-row",
            label(
              cls := "clock-label",
              child <-- opVar.signal.map(op => span(bLabel(op) + ":"))
            ),
            input(
              typ := "number",
              cls := "clock-number-input",
              value <-- bVar.signal.map(_.toString),
              onInput.mapToValue --> { v => bVar.set(v.toIntOption.getOrElse(1)) }
            )
          ),

          // View-mode toggle
          div(
            cls := "clock-mode-row",
            p(cls := "clock-label", "View"),
            child <-- modeVar.signal.map { mode =>
              div(
                cls := "clock-mode-buttons",
                button(
                  cls := "clock-mode-btn" + (if mode == "single" then " clock-mode-active" else ""),
                  "Single step",
                  onClick --> (_ => modeVar.set("single"))
                ),
                button(
                  cls := "clock-mode-btn" + (if mode == "pattern" then " clock-mode-active" else ""),
                  "Pattern view",
                  onClick --> (_ => modeVar.set("pattern"))
                )
              )
            }
          ),

          // Result / pattern description
          child <-- nVar.signal
            .combineWith(aVar.signal, bVar.signal, opVar.signal, modeVar.signal)
            .map { case (n, a, b, op, mode) =>
              val a2 = modN(a, n)
              if mode == "single" then
                val result = singleResult(a2, b, n, op)
                Callout(_.variant := "brand")(
                  cls := "clock-result-callout",
                  span(strong(exprLabel(a2, b, n, op))),
                  span(cls := "clock-result-eq", " = "),
                  span(cls := "clock-result-val", strong(result.toString))
                ): HtmlElement
              else
                Callout(_.variant := "neutral")(
                  cls := "clock-pattern-callout",
                  span("Pattern: "),
                  strong(patternDesc(a2, b, n, op))
                ): HtmlElement
            },

          // Sequence display (single mode only)
          child <-- seqSignal.combineWith(modeVar.signal).map { case (seq, mode) =>
            if mode == "single" then
              val cycleLen = seq.length - 1
              div(
                cls := "clock-sequence-section",
                p(cls := "clock-label", "Sequence:"),
                div(
                  cls := "clock-seq-row",
                  seq.zipWithIndex.map { case (v, i) =>
                    val isLast  = i == seq.length - 1
                    val isFirst = i == 0
                    span(
                      cls := (
                        if isFirst then "seq-chip seq-chip-start"
                        else if isLast then "seq-chip seq-chip-return"
                        else "seq-chip"
                      ),
                      v.toString
                    )
                  }
                ),
                p(cls := "clock-cycle-info", s"Cycle length: $cycleLen")
              ): HtmlElement
            else span(): HtmlElement
          }
        )
      ),

      // ── Discover Mode ─────────────────────────────────────────────────────
      div(
        cls := "clock-discover",
        h3("Discover Mode"),
        p(
          cls := "clock-discover-intro",
          "Instead of theory, try these — click a card and see what happens on the clock!"
        ),
        div(
          cls := "clock-discoveries",
          discoverCard(
            "🔄",
            "How long is the cycle?",
            "Set n = 12, keep adding 4. How many steps until you're back to 0?",
            () => { nVar.set(12); aVar.set(0); bVar.set(4); opVar.set("add"); modeVar.set("single") }
          ),
          discoverCard(
            "⭐",
            "Star polygons",
            "Set n = 12, multiplier = 5, switch to Pattern View. What shape appears?",
            () => { nVar.set(12); aVar.set(1); bVar.set(5); opVar.set("mul"); modeVar.set("pattern") }
          ),
          discoverCard(
            "🔑",
            "Powers cycle back",
            "Set n = 7, base = 3, Powers mode. Notice the cycle returns to 3 — the multiplicative order!",
            () => { nVar.set(7); aVar.set(3); bVar.set(6); opVar.set("pow"); modeVar.set("single") }
          ),
          discoverCard(
            "🌀",
            "Who visits everyone?",
            "For n = 9, try different step values in Addition mode (Pattern View). Which steps visit ALL positions?",
            () => { nVar.set(9); aVar.set(0); bVar.set(1); opVar.set("add"); modeVar.set("pattern") }
          )
        )
      )
    )

  private def discoverCard(
      emoji: String,
      title: String,
      description: String,
      action: () => Unit
  ): HtmlElement =
    Card(_.withHeader := true)(
      cls := "discover-card",
      div(
        slot := "header",
        span(cls := "discover-emoji", emoji),
        span(s" $title")
      ),
      p(description),
      Button(_.size := "small")(
        "Try it →",
        onClick --> (_ => action())
      )
    )

end ClockPage
