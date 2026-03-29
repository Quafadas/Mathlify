package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*
import mathlify.MathExpr

object MatrixPage:

  // ── Types ─────────────────────────────────────────────────────────────────
  case class CellRef(matrix: String, row: Int, col: Int)
  case class CalcDetail(row: Int, col: Int, terms: List[(Double, Double)], result: Double)
  case class ParsedMatrix(data: Vector[Double], rows: Int, cols: Int)

  // ── State ─────────────────────────────────────────────────────────────────
  private val hovered: Var[Option[CellRef]] = Var(None)
  private val selectedCalc: Var[Option[CalcDetail]] = Var(None)

  private val asciiA: Var[String] = Var("[(1,2,3),(4,5,6)]")
  private val asciiB: Var[String] = Var("[(7,8),(9,10),(11,12)]")

  // ── Parsing ───────────────────────────────────────────────────────────────

  private def parseMatrix(input: String): Either[String, ParsedMatrix] =
    mathlify.AsciiMath.translate(input.trim) match
      case Left(err) => Left(err)
      case Right(expr) =>
        extractMatrix(expr) match
          case Some(m) => Right(m)
          case None    => Left("Expression is not a matrix. Use e.g. [(1,2),(3,4)]")

  private def extractMatrix(expr: MathExpr): Option[ParsedMatrix] =
    expr match
      case MathExpr.BracketGroup(_, _, inner) => extractMatrix(inner)
      case MathExpr.Group(inner)              => extractMatrix(inner)
      case MathExpr.Matrix(elems, rows, cols, _, _, _) =>
        val nums = elems.map(evalToDouble)
        if nums.forall(_.isDefined) then Some(ParsedMatrix(nums.map(_.get).toVector, rows, cols))
        else None
      case _ => None

  private def evalToDouble(expr: MathExpr): Option[Double] =
    expr match
      case MathExpr.Number(v) => Some(v)
      case MathExpr.Neg(MathExpr.Number(v)) => Some(-v)
      case MathExpr.Group(e) => evalToDouble(e)
      case _ =>
        mathlify.Evaluator.eval(expr) match
          case mathlify.Numeric(v) => Some(v)
          case _                   => None

  // ── Matrix arithmetic ─────────────────────────────────────────────────────

  private def getCell(mat: Vector[Double], cols: Int, r: Int, c: Int): Double =
    val idx = r * cols + c
    if idx >= 0 && idx < mat.size then mat(idx) else 0.0

  private def multiply(a: ParsedMatrix, b: ParsedMatrix): ParsedMatrix =
    val result = Vector.newBuilder[Double]
    for r <- 0 until a.rows do
      for c <- 0 until b.cols do
        var sum = 0.0
        for k <- 0 until a.cols do sum += getCell(a.data, a.cols, r, k) * getCell(b.data, b.cols, k, c)
        result += sum
    ParsedMatrix(result.result(), a.rows, b.cols)

  // ── Serialize back to AsciiMath ───────────────────────────────────────────

  private def toAscii(m: ParsedMatrix): String =
    val rows = (0 until m.rows).map { r =>
      val cells = (0 until m.cols).map { c => formatCell(getCell(m.data, m.cols, r, c)) }
      s"(${cells.mkString(",")})"
    }
    s"[${rows.mkString(",")}]"

  // ── Render ────────────────────────────────────────────────────────────────

  private def mathml(ascii: String): HtmlElement =
    mathlify.AsciiMath.translate(ascii) match
      case Right(expr) => mathlify.LaminarRenderer.render(expr)
      case Left(_)     => span(ascii)

  def render(): HtmlElement =
    selectedCalc.set(None)
    hovered.set(None)

    val parsedA = asciiA.signal.map(parseMatrix)
    val parsedB = asciiB.signal.map(parseMatrix)

    val resultSignal = parsedA.combineWith(parsedB).map {
      case (Right(a), Right(b)) if a.cols == b.rows => Right(multiply(a, b))
      case (Right(a), Right(b))                     => Left(s"Dimension mismatch: A is ${a.rows}×${a.cols} but B is ${b.rows}×${b.cols}")
      case (Left(e), _)                             => Left(s"Matrix A: $e")
      case (_, Left(e))                             => Left(s"Matrix B: $e")
    }

    div(
      cls := "matrix-page",
      h2("Interactive Matrix Multiplication"),
      p(
        cls := "matrix-intro",
        "Enter matrices as AsciiMath — e.g. ",
        code("[(1,2,3),(4,5,6)]"),
        ". Hover over cells to highlight rows/columns. Click an output cell to see the dot-product calculation."
      ),
      div(
        cls := "matrix-ascii-inputs",
        div(
          cls := "matrix-input-group",
          span(cls := "matrix-label", "A"),
          Textarea(_.label := "Matrix A", _.rows := 3)(
            value <-- asciiA.signal,
            onInput.mapToValue --> asciiA.writer
          ),
          div(
            cls := "matrix-preview",
            child <-- asciiA.signal.map(mathml)
          )
        ),
        div(
          cls := "matrix-input-group",
          span(cls := "matrix-label", "B"),
          Textarea(_.label := "Matrix B", _.rows := 3)(
            value <-- asciiB.signal,
            onInput.mapToValue --> asciiB.writer
          ),
          div(
            cls := "matrix-preview",
            child <-- asciiB.signal.map(mathml)
          )
        )
      ),
      div(
        cls := "dim-check",
        child <-- parsedA.combineWith(parsedB).map {
          case (Right(a), Right(b)) =>
            val compatible = a.cols == b.rows
            val msg =
              if compatible then s"A is ${a.rows}×${a.cols}, B is ${b.rows}×${b.cols} — inner dimensions match (${a.cols} = ${b.rows}). Result will be ${a.rows}×${b.cols}."
              else s"A is ${a.rows}×${a.cols}, B is ${b.rows}×${b.cols} — inner dimensions don't match (${a.cols} ≠ ${b.rows})."
            if compatible then
              Callout(_.variant := "success")(
                Icon()("check"),
                span(s" $msg")
              ): HtmlElement
            else
              Callout(_.variant := "danger")(
                Icon()("x"),
                span(s" $msg")
              ): HtmlElement
          case (Left(_), _) | (_, Left(_)) =>
            span(): HtmlElement
        }
      ),
      child <-- resultSignal.combineWith(parsedA, parsedB).map {
        case (Left(err), _, _) =>
          Callout(_.variant := "danger")(err): HtmlElement
        case (Right(c), Right(a), Right(b)) =>
          div(
            cls := "matrix-layout",
            editableGrid("A", a, asciiA),
            span(cls := "matrix-op", "×"),
            editableGrid("B", b, asciiB),
            span(cls := "matrix-op", "="),
            resultGrid(c, a, b)
          ): HtmlElement
        case _ => span(): HtmlElement
      },
      div(
        cls := "calc-detail",
        child <-- selectedCalc.signal.map {
          case None => span(): HtmlElement
          case Some(detail) =>
            Card(_.withHeader := true)(
              div(
                slot := "header",
                Icon()("calculator"),
                span(s" C(${detail.row + 1}, ${detail.col + 1}) calculation")
              ),
              div(
                cls := "calc-terms",
                detail.terms.zipWithIndex.map { case ((aVal, bVal), k) =>
                  span(
                    cls := "calc-term",
                    if k > 0 then " + " else "",
                    span(cls := "calc-a", f"$aVal%.4g"),
                    " × ",
                    span(cls := "calc-b", f"$bVal%.4g")
                  )
                },
                span(cls := "calc-eq", s" = "),
                span(cls := "calc-result", f"${detail.result}%.4g")
              )
            ): HtmlElement
        }
      )
    )
  end render

  // ── Editable input grid (hoverable, writes back to ascii Var) ─────────────

  private def editableGrid(name: String, m: ParsedMatrix, asciiVar: Var[String]): HtmlElement =
    div(
      cls := "matrix-wrapper",
      span(cls := "matrix-label", name),
      div(
        cls := "matrix-bracket",
        div(
          cls := "matrix-grid",
          styleAttr := s"grid-template-columns: repeat(${m.cols}, 1fr);",
          (0 until m.rows).flatMap { r =>
            (0 until m.cols).map { c =>
              val v = getCell(m.data, m.cols, r, c)
              val cellRef = CellRef(name, r, c)
              input(
                cls := "matrix-cell editable-cell",
                cls <-- highlightClass(cellRef),
                typ := "number",
                defaultValue := formatCell(v),
                onInput.mapToValue --> { s =>
                  s.toDoubleOption.foreach { d =>
                    val idx = r * m.cols + c
                    val updated = m.copy(data = m.data.updated(idx, d))
                    asciiVar.set(toAscii(updated))
                    selectedCalc.set(None)
                  }
                },
                onMouseEnter.mapTo(Some(cellRef)) --> hovered.writer,
                onMouseLeave.mapTo(None) --> hovered.writer,
                styleAttr := s"grid-column: ${c + 1}; grid-row: ${r + 1};"
              )
            }
          }
        )
      )
    )

  // ── Result grid (clickable) ───────────────────────────────────────────────

  private def resultGrid(c: ParsedMatrix, a: ParsedMatrix, b: ParsedMatrix): HtmlElement =
    div(
      cls := "matrix-wrapper",
      span(cls := "matrix-label", "C"),
      div(
        cls := "matrix-bracket",
        div(
          cls := "matrix-grid",
          styleAttr := s"grid-template-columns: repeat(${c.cols}, 1fr);",
          (0 until c.rows).flatMap { r =>
            (0 until c.cols).map { col =>
              val v = getCell(c.data, c.cols, r, col)
              val cellRef = CellRef("C", r, col)
              div(
                cls := "matrix-cell result-cell",
                cls <-- highlightClass(cellRef),
                formatCell(v),
                onMouseEnter.mapTo(Some(cellRef)) --> hovered.writer,
                onMouseLeave.mapTo(None) --> hovered.writer,
                onClick --> { _ =>
                  val terms = (0 until a.cols).map { k =>
                    (getCell(a.data, a.cols, r, k), getCell(b.data, b.cols, k, col))
                  }.toList
                  selectedCalc.set(Some(CalcDetail(r, col, terms, v)))
                },
                styleAttr := s"grid-column: ${col + 1}; grid-row: ${r + 1}; cursor: pointer;"
              )
            }
          }
        )
      )
    )

  // ── Highlight logic ───────────────────────────────────────────────────────

  private def highlightClass(cell: CellRef): Signal[Seq[String]] =
    hovered.signal.map {
      case None => Seq.empty
      case Some(h) =>
        cell.matrix match
          case "A" =>
            h match
              case CellRef("A", r, _) if r == cell.row => Seq("highlight-row")
              case CellRef("A", _, c) if c == cell.col => Seq("highlight-col")
              case CellRef("C", r, _) if r == cell.row => Seq("highlight-row")
              case _                                    => Seq.empty
          case "B" =>
            h match
              case CellRef("B", r, _) if r == cell.row => Seq("highlight-row")
              case CellRef("B", _, c) if c == cell.col => Seq("highlight-col")
              case CellRef("C", _, c) if c == cell.col => Seq("highlight-col")
              case _                                    => Seq.empty
          case "C" =>
            h match
              case CellRef("A", r, _) if r == cell.row                  => Seq("highlight-row")
              case CellRef("B", _, c) if c == cell.col                  => Seq("highlight-col")
              case CellRef("C", r, c) if r == cell.row && c == cell.col => Seq("highlight-cell")
              case CellRef("C", r, _) if r == cell.row                  => Seq("highlight-row")
              case CellRef("C", _, c) if c == cell.col                  => Seq("highlight-col")
              case _                                                     => Seq.empty
          case _ => Seq.empty
    }

  private def formatCell(d: Double): String =
    if d == d.toLong.toDouble then d.toLong.toString else f"$d%.4g"

end MatrixPage
