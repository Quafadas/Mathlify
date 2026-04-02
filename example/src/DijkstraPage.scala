package mathlify.example

import com.raquo.laminar.api.L.{*, given}
import io.github.nguyenyou.webawesome.laminar.*

object DijkstraPage:

  // ── Graph definition ───────────────────────────────────────────────────────
  private case class Node(id: Int, label: String, x: Double, y: Double)
  private case class Edge(from: Int, to: Int, weight: Int)

  private val SVG_W = 520.0
  private val SVG_H = 370.0
  private val NODE_R = 18.0

  private val nodes: List[Node] = List(
    Node(0, "S", 80, 185),
    Node(1, "A", 220, 75),
    Node(2, "B", 360, 75),
    Node(3, "C", 360, 295),
    Node(4, "D", 220, 295),
    Node(5, "E", 460, 185)
  )

  // Undirected weighted edges
  private val edges: List[Edge] = List(
    Edge(0, 1, 7), // S–A
    Edge(0, 4, 2), // S–D
    Edge(1, 2, 4), // A–B
    Edge(1, 4, 6), // A–D
    Edge(2, 3, 2), // B–C
    Edge(2, 5, 3), // B–E
    Edge(3, 4, 3), // C–D
    Edge(3, 5, 1), // C–E
    Edge(4, 5, 8) // D–E
  )

  // ── Algorithm state ────────────────────────────────────────────────────────
  private case class StepState(
      description: String,
      distances: Map[Int, Option[Int]], // None = ∞
      visited: Set[Int],
      current: Option[Int]
  )

  private val INF: Option[Int] = None

  // Pre-computed steps for Dijkstra from S (node 0)
  private val steps: Vector[StepState] = Vector(
    StepState(
      description = "Initial state. The source node S has distance 0; all other nodes have distance ∞. No nodes have been visited yet.",
      distances = Map(0 -> Some(0), 1 -> INF, 2 -> INF, 3 -> INF, 4 -> INF, 5 -> INF),
      visited = Set.empty,
      current = None
    ),
    StepState(
      description = "Visit S (distance 0) — the node with the smallest tentative distance. Relax outgoing edges: A becomes 7 (0 + 7), D becomes 2 (0 + 2).",
      distances = Map(0 -> Some(0), 1 -> Some(7), 2 -> INF, 3 -> INF, 4 -> Some(2), 5 -> INF),
      visited = Set(0),
      current = Some(0)
    ),
    StepState(
      description = "Visit D (distance 2) — smallest unvisited distance. Relax edges: C becomes 5 (2 + 3), E becomes 10 (2 + 8). A stays 7 because 2 + 6 = 8 > 7.",
      distances = Map(0 -> Some(0), 1 -> Some(7), 2 -> INF, 3 -> Some(5), 4 -> Some(2), 5 -> Some(10)),
      visited = Set(0, 4),
      current = Some(4)
    ),
    StepState(
      description = "Visit C (distance 5) — smallest unvisited distance. Relax edges: B becomes 7 (5 + 2), E improves to 6 (5 + 1 < 10).",
      distances = Map(0 -> Some(0), 1 -> Some(7), 2 -> Some(7), 3 -> Some(5), 4 -> Some(2), 5 -> Some(6)),
      visited = Set(0, 4, 3),
      current = Some(3)
    ),
    StepState(
      description = "Visit E (distance 6) — smallest unvisited distance. Relax edges: B stays 7 because 6 + 3 = 9 > 7. No improvements.",
      distances = Map(0 -> Some(0), 1 -> Some(7), 2 -> Some(7), 3 -> Some(5), 4 -> Some(2), 5 -> Some(6)),
      visited = Set(0, 4, 3, 5),
      current = Some(5)
    ),
    StepState(
      description = "Visit A (distance 7) — tied smallest unvisited distance. Relax edges: B stays 7 because 7 + 4 = 11 > 7. No improvements.",
      distances = Map(0 -> Some(0), 1 -> Some(7), 2 -> Some(7), 3 -> Some(5), 4 -> Some(2), 5 -> Some(6)),
      visited = Set(0, 4, 3, 5, 1),
      current = Some(1)
    ),
    StepState(
      description = "Visit B (distance 7) — all neighbours already visited. Algorithm complete! Shortest distances from S: A = 7, B = 7, C = 5, D = 2, E = 6.",
      distances = Map(0 -> Some(0), 1 -> Some(7), 2 -> Some(7), 3 -> Some(5), 4 -> Some(2), 5 -> Some(6)),
      visited = Set(0, 4, 3, 5, 1, 2),
      current = Some(2)
    )
  )

  // ── Reactive state ─────────────────────────────────────────────────────────
  private val stepVar = Var(0)

  // ── SVG helpers ────────────────────────────────────────────────────────────
  private def fmt(d: Double): String = f"$d%.2f"

  private def nodeColor(id: Int, state: StepState): String =
    if state.current.contains(id) then "#f97316" // orange = currently processing
    else if state.visited.contains(id) then "#22c55e" // green = visited
    else if state.distances(id).isDefined then "#93c5fd" // blue = in queue
    else "#e2e8f0" // gray = not yet reached

  private def nodeStroke(id: Int, state: StepState): String =
    if state.current.contains(id) then "#c2410c"
    else if state.visited.contains(id) then "#15803d"
    else if state.distances(id).isDefined then "#2563eb"
    else "#94a3b8"

  // Determine edge midpoint and offset for weight label
  private def edgeMid(from: Node, to: Node): (Double, Double) =
    ((from.x + to.x) / 2, (from.y + to.y) / 2)

  // Shorten a line segment so it starts/ends outside node circles
  private def shortenPt(ax: Double, ay: Double, bx: Double, by: Double, d: Double): (Double, Double) =
    val dx = bx - ax; val dy = by - ay
    val len = Math.sqrt(dx * dx + dy * dy)
    if len < 0.01 then (ax, ay)
    else (ax + dx / len * d, ay + dy / len * d)
    end if
  end shortenPt

  // ── SVG builder ────────────────────────────────────────────────────────────
  private def buildSvg(state: StepState): HtmlElement =
    import com.raquo.laminar.api.L.svg as S

    val nodeById = nodes.map(n => n.id -> n).toMap

    // Edge elements
    val edgeElems = edges.flatMap { e =>
      val from = nodeById(e.from)
      val to = nodeById(e.to)
      val (sx, sy) = shortenPt(from.x, from.y, to.x, to.y, NODE_R + 2)
      val (ex, ey) = shortenPt(to.x, to.y, from.x, from.y, NODE_R + 2)

      // Highlight edge if one endpoint is current and the other is unvisited/in-queue
      val isCurrent = state.current.exists(c => c == e.from || c == e.to)
      val edgeColor = if isCurrent then "#3b82f6" else "#cbd5e1"
      val strokeW = if isCurrent then "2.5" else "1.8"

      val (mx, my) = edgeMid(from, to)
      // Perpendicular offset for label to avoid overlap with the line
      val dx = to.x - from.x; val dy = to.y - from.y
      val len = Math.sqrt(dx * dx + dy * dy)
      val ox = if len < 0.01 then 0.0 else -dy / len * 11.0
      val oy = if len < 0.01 then 0.0 else dx / len * 11.0

      List(
        S.line(
          S.x1 := fmt(sx),
          S.y1 := fmt(sy),
          S.x2 := fmt(ex),
          S.y2 := fmt(ey),
          S.style := s"stroke: $edgeColor; stroke-width: ${strokeW}px;"
        ),
        S.text(
          S.x := fmt(mx + ox),
          S.y := fmt(my + oy),
          S.style := "text-anchor: middle; dominant-baseline: central; font-size: 12px; fill: #475569; font-weight: 600;",
          TextNode(e.weight.toString)
        )
      )
    }

    // Node circle + label elements
    val nodeElems = nodes.flatMap { n =>
      val fill = nodeColor(n.id, state)
      val stroke = nodeStroke(n.id, state)
      List(
        S.circle(
          S.cx := fmt(n.x),
          S.cy := fmt(n.y),
          S.r := fmt(NODE_R),
          S.style := s"fill: $fill; stroke: $stroke; stroke-width: 2.5px;"
        ),
        S.text(
          S.x := fmt(n.x),
          S.y := fmt(n.y),
          S.style := "text-anchor: middle; dominant-baseline: central; font-size: 13px; fill: #1e293b; font-weight: bold;",
          TextNode(n.label)
        )
      )
    }

    val svgElem = S.svg(
      S.viewBox := s"0 0 ${fmt(SVG_W)} ${fmt(SVG_H)}",
      S.style := s"width: 100%; max-width: ${SVG_W.toInt}px; height: auto; display: block;",
      edgeElems,
      nodeElems
    )
    div(cls := "dijkstra-svg-container", svgElem)
  end buildSvg

  // ── Distance table ─────────────────────────────────────────────────────────
  private def distTable(state: StepState): HtmlElement =
    val rows = nodes.map { n =>
      val distStr = state.distances(n.id) match
        case Some(d) => d.toString
        case None    => "∞"
      val statusCls =
        if state.current.contains(n.id) then "dijkstra-dist-current"
        else if state.visited.contains(n.id) then "dijkstra-dist-visited"
        else if state.distances(n.id).isDefined then "dijkstra-dist-queued"
        else "dijkstra-dist-unreached"
      tr(
        cls := statusCls,
        td(cls := "dijkstra-td-node", n.label),
        td(cls := "dijkstra-td-dist", distStr)
      )
    }
    table(
      cls := "dijkstra-dist-table",
      thead(tr(th("Node"), th("Distance"))),
      tbody(rows)
    )
  end distTable

  // ── Legend ─────────────────────────────────────────────────────────────────
  private def legend: HtmlElement =
    div(
      cls := "dijkstra-legend",
      div(cls := "dijkstra-legend-item", div(cls := "dijkstra-legend-dot dijkstra-legend-source"), span("Visited")),
      div(cls := "dijkstra-legend-item", div(cls := "dijkstra-legend-dot dijkstra-legend-current"), span("Currently visiting")),
      div(cls := "dijkstra-legend-item", div(cls := "dijkstra-legend-dot dijkstra-legend-queued"), span("In priority queue")),
      div(cls := "dijkstra-legend-item", div(cls := "dijkstra-legend-dot dijkstra-legend-unreached"), span("Not yet reached"))
    )

  // ── Page render ────────────────────────────────────────────────────────────
  def render(): HtmlElement =
    val maxStep = steps.length - 1

    val stateSignal = stepVar.signal.map(s => steps(s.min(maxStep).max(0)))

    div(
      cls := "dijkstra-page",
      h2("Dijkstra's Shortest-Path Algorithm"),
      p(
        cls := "dijkstra-intro",
        "Dijkstra's algorithm finds the shortest path from a source node to every other node in a weighted graph. ",
        "Step through the algorithm below to see how it greedily picks the nearest unvisited node and ",
        strong("relaxes"),
        " its edges."
      ),
      div(
        cls := "dijkstra-main-layout",
        // ── Left: SVG ─────────────────────────────────────────────────────
        div(
          cls := "dijkstra-visual-panel",
          child <-- stateSignal.map(buildSvg),
          legend
        ),
        // ── Right: description + table ─────────────────────────────────
        div(
          cls := "dijkstra-info-panel",
          // Step counter
          div(
            cls := "dijkstra-step-counter",
            child <-- stepVar.signal.map(s => span(if s == 0 then "Initial State" else s"Step $s / $maxStep"))
          ),
          // Step description
          div(
            cls := "dijkstra-step-desc",
            child <-- stateSignal.map(s => p(s.description))
          ),
          // Distance table
          h3(cls := "dijkstra-table-heading", "Tentative Distances"),
          child <-- stateSignal.map(distTable)
        )
      ),
      // ── Controls ────────────────────────────────────────────────────────
      div(
        cls := "dijkstra-controls",
        button(
          cls := "dijkstra-ctrl-btn",
          "← Prev",
          disabled <-- stepVar.signal.map(_ <= 0),
          onClick --> (_ => stepVar.update(s => (s - 1).max(0)))
        ),
        button(
          cls := "dijkstra-ctrl-btn",
          "Next →",
          disabled <-- stepVar.signal.map(_ >= maxStep),
          onClick --> (_ => stepVar.update(s => (s + 1).min(maxStep)))
        ),
        button(
          cls := "dijkstra-ctrl-btn dijkstra-ctrl-reset",
          "Reset",
          onClick --> (_ => stepVar.set(0))
        )
      )
    )
  end render

end DijkstraPage
