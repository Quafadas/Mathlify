package mathlify.example

import com.raquo.laminar.api.L.*
import io.github.nguyenyou.webawesome.laminar.*

object RekenrekPage:

  private val BEADS_PER_ROW = 10

  // State: count of active (left-pushed) beads per row (0–10)
  private val rowCounts: Var[Vector[Int]] = Var(Vector(0, 0))

  def render(): HtmlElement =
    rowCounts.set(Vector(0, 0))

    div(
      cls := "rekenrek-page",
      h2("Interactive Rekenrek"),
      p(
        cls := "rekenrek-intro",
        "A Rekenrek is a counting frame that makes adding easy! Push beads to the left to count them. Each row has 10 beads — 5 red and 5 blue."
      ),
      div(
        cls := "rekenrek-frame",
        renderRow(0),
        renderRow(1)
      ),
      div(
        cls := "rekenrek-total-section",
        child <-- rowCounts.signal.map { counts =>
          val r1 = counts(0)
          val r2 = counts(1)
          val total = r1 + r2
          if r1 > 0 || r2 > 0 then
            Callout(_.variant := "success")(
              cls := "rekenrek-total-callout",
              span(
                cls := "rekenrek-eq-text",
                if r1 > 0 && r2 > 0 then s"$r1 + $r2 = $total"
                else s"Total: $total"
              )
            ): HtmlElement
          else
            Callout(_.variant := "neutral")(
              cls := "rekenrek-total-callout",
              "Click beads to start counting!"
            ): HtmlElement
          end if
        }
      ),
      Button()(
        cls := "rekenrek-reset-btn",
        "Reset",
        onClick --> { _ => rowCounts.set(Vector(0, 0)) }
      )
    )
  end render

  private def renderRow(rowIdx: Int): HtmlElement =
    div(
      cls := "rekenrek-row",
      div(
        cls := "rekenrek-row-header",
        span(cls := "rekenrek-row-label", s"Row ${rowIdx + 1}:"),
        child <-- rowCounts.signal.map(counts => span(cls := "rekenrek-row-count", counts(rowIdx).toString))
      ),
      div(
        cls := "rekenrek-rod-container",
        div(cls := "rekenrek-rod"),
        div(
          cls := "rekenrek-beads-row",
          // Active beads (pushed left)
          div(
            cls := "rekenrek-bead-group rekenrek-active-group",
            children <-- rowCounts.signal.map { counts =>
              val count = counts(rowIdx)
              (0 until count).map { i =>
                beadEl(rowIdx, i, active = true)
              }.toList
            }
          ),
          // Spacer between active and inactive
          div(cls := "rekenrek-bead-spacer"),
          // Inactive beads (pushed right)
          div(
            cls := "rekenrek-bead-group rekenrek-inactive-group",
            children <-- rowCounts.signal.map { counts =>
              val count = counts(rowIdx)
              (count until BEADS_PER_ROW).map { i =>
                beadEl(rowIdx, i, active = false)
              }.toList
            }
          )
        )
      )
    )
  end renderRow

  private def beadEl(rowIdx: Int, beadIdx: Int, active: Boolean): HtmlElement =
    val colorCls =
      if active then (if beadIdx < 5 then "bead-red" else "bead-blue")
      else (if beadIdx < 5 then "bead-inactive-red"
            else "bead-inactive-blue")
    div(
      cls := s"rekenrek-bead $colorCls",
      onClick --> { _ =>
        rowCounts.update { counts =>
          val newCount =
            if active then beadIdx // deactivate this bead and all to its right
            else beadIdx + 1 // activate this bead and all to its left
          counts.updated(rowIdx, newCount)
        }
      }
    )
  end beadEl

end RekenrekPage
