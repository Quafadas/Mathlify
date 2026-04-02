package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*

object ArrayBoardPage:

  private val BOARD_SIZE = 10

  case class PlacedArray(startRow: Int, startCol: Int, rows: Int, cols: Int, colorIdx: Int):
    def covers(r: Int, c: Int): Boolean =
      r >= startRow && r < startRow + rows && c >= startCol && c < startCol + cols
  end PlacedArray

  private val placed: Var[List[PlacedArray]] = Var(List.empty)
  private val selectedRows: Var[Int] = Var(3)
  private val selectedCols: Var[Int] = Var(4)
  private val hoverCell: Var[Option[(Int, Int)]] = Var(None)
  private val targetNumber: Var[Int] = Var(12)
  private val nextColorIdx: Var[Int] = Var(0)
  private val newScore: Var[Int] = Var(5)
  private val duplicateScore: Var[Int] = Var(2)

  private val colors = Vector(
    "#e57373",
    "#64b5f6",
    "#81c784",
    "#ffb74d",
    "#ce93d8",
    "#4dd0e1",
    "#aed581",
    "#ffcc80",
    "#9575cd",
    "#f06292"
  )

  private val challengeTargets = Vector(6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 30, 36)

  private def randomTarget(): Int =
    challengeTargets(scala.util.Random.nextInt(challengeTargets.size))

  /** Returns (points, isDuplicate) for each placed array in order, using O(n) Set tracking. */
  private def scoredArrays(placedList: List[PlacedArray], ns: Int, ds: Int): List[(PlacedArray, Int, Boolean)] =
    placedList
      .foldLeft((List.empty[(PlacedArray, Int, Boolean)], Set.empty[(Int, Int)])) { case ((acc, seen), arr) =>
        val key = (arr.rows, arr.cols)
        val isDup = seen.contains(key)
        val pts = if isDup then ds else ns
        (acc :+ (arr, pts, isDup), seen + key)
      }
      ._1

  private def computeScore(placedList: List[PlacedArray], ns: Int, ds: Int): Int =
    scoredArrays(placedList, ns, ds).map(_._2).sum

  private def canPlace(
      startRow: Int,
      startCol: Int,
      rows: Int,
      cols: Int,
      placedList: List[PlacedArray]
  ): Boolean =
    startRow >= 0 && startCol >= 0 &&
      startRow + rows <= BOARD_SIZE &&
      startCol + cols <= BOARD_SIZE &&
      !placedList.exists(arr => (0 until rows).exists(dr => (0 until cols).exists(dc => arr.covers(startRow + dr, startCol + dc))))

  def render(): HtmlElement =
    placed.set(List.empty)
    hoverCell.set(None)
    selectedRows.set(3)
    selectedCols.set(4)
    nextColorIdx.set(0)
    targetNumber.set(randomTarget())

    val scoreSignal =
      placed.signal.combineWith(newScore.signal, duplicateScore.signal).map { (ps, ns, ds) =>
        computeScore(ps, ns, ds)
      }

    div(
      cls := "array-board-page",
      h2("Array Board Game"),
      p(
        cls := "array-board-intro",
        "Build rectangular arrays on the grid! Choose rows and columns, hover to preview, then click to place. A new array scores more than a repeated one."
      ),
      child <-- targetNumber.signal.map { target =>
        Callout(_.variant := "neutral")(
          cls := "array-board-target",
          b(s"Challenge: Make $target! "),
          span(s"Place arrays that cover exactly $target squares. How many different ways can you find?")
        ): HtmlElement
      },
      // Dimension selectors
      div(
        cls := "array-board-controls",
        div(
          cls := "array-board-selector",
          label(cls := "array-dim-label", "Rows:"),
          input(
            typ := "number",
            cls := "array-dim-input",
            minAttr := "1",
            maxAttr := "10",
            value <-- selectedRows.signal.map(_.toString),
            onInput.mapToValue.map(s => s.toIntOption.map(_.max(1).min(BOARD_SIZE)).getOrElse(1)) --> selectedRows.writer
          )
        ),
        div(
          cls := "array-board-selector",
          label(cls := "array-dim-label", "Cols:"),
          input(
            typ := "number",
            cls := "array-dim-input",
            minAttr := "1",
            maxAttr := "10",
            value <-- selectedCols.signal.map(_.toString),
            onInput.mapToValue.map(s => s.toIntOption.map(_.max(1).min(BOARD_SIZE)).getOrElse(1)) --> selectedCols.writer
          )
        ),
        child <-- selectedRows.signal.combineWith(selectedCols.signal).map { (r, c) =>
          span(cls := "array-current-selection", s"$r × $c = ${r * c}"): HtmlElement
        }
      ),
      // Score configuration
      div(
        cls := "array-board-score-config",
        div(
          cls := "array-board-selector",
          label(cls := "array-dim-label", "New array pts:"),
          input(
            typ := "number",
            cls := "array-dim-input",
            minAttr := "0",
            maxAttr := "100",
            value <-- newScore.signal.map(_.toString),
            onInput.mapToValue.map(s => s.toIntOption.map(_.max(0).min(100)).getOrElse(5)) --> newScore.writer
          )
        ),
        div(
          cls := "array-board-selector",
          label(cls := "array-dim-label", "Duplicate pts:"),
          input(
            typ := "number",
            cls := "array-dim-input",
            minAttr := "0",
            maxAttr := "100",
            value <-- duplicateScore.signal.map(_.toString),
            onInput.mapToValue.map(s => s.toIntOption.map(_.max(0).min(100)).getOrElse(2)) --> duplicateScore.writer
          )
        )
      ),
      // Board grid
      div(
        cls := "array-board-wrapper",
        div(
          cls := "array-board-grid",
          onMouseLeave.mapTo(None) --> hoverCell.writer,
          (0 until BOARD_SIZE * BOARD_SIZE).map { idx =>
            val r = idx / BOARD_SIZE
            val c = idx % BOARD_SIZE

            val combinedSig =
              placed.signal.combineWith(hoverCell.signal, selectedRows.signal, selectedCols.signal)

            div(
              cls := "array-board-cell",
              cls <-- combinedSig.map { case (placedList, hover, rows, cols) =>
                val arrayIdx = placedList.indexWhere(_.covers(r, c))
                if arrayIdx >= 0 then Seq("array-cell-placed")
                else
                  hover match
                    case Some((hr, hc)) if r >= hr && r < hr + rows && c >= hc && c < hc + cols =>
                      if canPlace(hr, hc, rows, cols, placedList) then Seq("array-cell-preview")
                      else Seq("array-cell-preview-invalid")
                    case _ => Seq.empty
                end if
              },
              styleAttr <-- placed.signal.map { placedList =>
                val arrayIdx = placedList.indexWhere(_.covers(r, c))
                if arrayIdx >= 0 then s"background-color: ${colors(placedList(arrayIdx).colorIdx % colors.size)};"
                else ""
                end if
              },
              onMouseEnter.mapTo(Some((r, c))) --> hoverCell.writer,
              onClick --> { _ =>
                val currentPlaced = placed.now()
                val currentRows = selectedRows.now()
                val currentCols = selectedCols.now()
                if canPlace(r, c, currentRows, currentCols, currentPlaced) then
                  val colorIdx = nextColorIdx.now()
                  placed.update(_ :+ PlacedArray(r, c, currentRows, currentCols, colorIdx))
                  nextColorIdx.update(i => (i + 1) % colors.size)
                end if
              }
            )
          }.toList
        )
      ),
      // Score callout
      div(
        cls := "array-board-score-section",
        child <-- scoreSignal
          .combineWith(placed.signal, targetNumber.signal, newScore.signal, duplicateScore.signal)
          .map { case (score, placedList, target, ns, ds) =>
            val hits = placedList.count(a => a.rows * a.cols == target)
            if placedList.isEmpty then
              Callout(_.variant := "neutral")(
                cls := "array-board-score",
                "Hover over the grid to preview your array, then click to place it!"
              ): HtmlElement
            else
              Callout(_.variant := "success")(
                cls := "array-board-score",
                b(s"Score: $score"),
                span(s" (new: ${ns}pts, duplicate: ${ds}pts)"),
                if hits > 0 then span(s" — $hits way${if hits == 1 then "" else "s"} to make $target! 🎉")
                else span(s" — Keep going, can you make $target?")
              ): HtmlElement
            end if
          }
      ),
      // Placed arrays list
      child <-- placed.signal.combineWith(newScore.signal, duplicateScore.signal).map { case (placedList, ns, ds) =>
        if placedList.isEmpty then div(): HtmlElement
        else
          div(
            cls := "array-facts-list",
            h3("Arrays placed:"),
            scoredArrays(placedList, ns, ds).map { case (arr, pts, isDuplicate) =>
              val ptsCls =
                if isDuplicate then "array-fact-pts array-fact-pts-dup"
                else "array-fact-pts array-fact-pts-new"
              div(
                cls := "array-fact-item",
                div(
                  cls := "array-fact-swatch",
                  styleAttr := s"background-color: ${colors(arr.colorIdx % colors.size)};"
                ),
                span(s"${arr.rows} × ${arr.cols} = ${arr.rows * arr.cols}"),
                span(cls := ptsCls, s" +$pts")
              )
            }
          ): HtmlElement
      },
      // Action buttons
      div(
        cls := "array-board-buttons",
        Button()(
          "Reset Board",
          onClick --> { _ =>
            placed.set(List.empty)
            hoverCell.set(None)
            nextColorIdx.set(0)
          }
        ),
        Button()(
          "New Challenge",
          onClick --> { _ =>
            placed.set(List.empty)
            hoverCell.set(None)
            nextColorIdx.set(0)
            targetNumber.set(randomTarget())
          }
        )
      )
    )
  end render

end ArrayBoardPage
