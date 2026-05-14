package mathlify.example

import com.raquo.laminar.api.L.{*, given}
import io.github.nguyenyou.webawesome.laminar.*

object TimesTablePage:

  private case class Question(left: Int, right: Int):
    def product: Int = left * right
    def prompt: String = s"$left × $right"
  end Question

  private case class Submission(answerText: String):
    val parsed: Option[Int] = answerText.toIntOption
  end Submission

  private val modeVar = Var("series")
  private val tableVar = Var(2)
  private val answerVar = Var("")
  private val questionVar = Var(Question(2, 3))
  private val submissionVar = Var(Option.empty[Submission])

  private val palette = Vector(
    "#2563eb",
    "#ef4444",
    "#10b981",
    "#f59e0b",
    "#8b5cf6",
    "#ec4899",
    "#14b8a6",
    "#f97316",
    "#6366f1",
    "#84cc16",
    "#0ea5e9",
    "#f43f5e"
  )
  private val dotsPerTenGroup = 10
  private val dotsPerRow = 5
  private val waldorfLabelYOffset = 0.8
  private val roughDotRoughness = 2.1
  private val roughDotBowing = 1.4

  private def randomFactor(): Int = scala.util.Random.between(1, 13)

  private def nextQuestion(mode: String, table: Int): Question =
    mode match
      case "quiz" => Question(randomFactor(), table)
      case _      => Question(randomFactor(), randomFactor())
  end nextQuestion

  private def resetQuestion(mode: String): Unit =
    questionVar.set(nextQuestion(mode, tableVar.now()))
    answerVar.set("")
    submissionVar.set(None)
  end resetQuestion

  private def activateMode(mode: String): Unit =
    modeVar.set(mode)
    if mode == "series" then
      answerVar.set("")
      submissionVar.set(None)
    else resetQuestion(mode)
    end if
  end activateMode

  private def checkAnswer(): Unit =
    submissionVar.set(Some(Submission(answerVar.now().trim)))
  end checkAnswer

  private def modeButton(mode: String, label: String): HtmlElement =
    button(
      typ := "button",
      cls := "times-mode-btn",
      cls.toggle("times-mode-btn-active") <-- modeVar.signal.map(_ == mode),
      label,
      onClick --> (_ => activateMode(mode))
    )
  end modeButton

  private def renderSeries(): HtmlElement =
    div(
      cls := "times-series-section",
      child <-- tableVar.signal.map { table =>
        val products = (1 to 12).map(_ * table)
        div(
          Callout(_.variant := "neutral")(
            cls := "times-overview-callout",
            b(s"The $table times table"),
            span(s" builds by counting in steps of $table: ${products.mkString(", ")}.")
          ),
          ul(
            cls := "times-series-list",
            (1 to 12).map { n =>
              li(
                cls := "times-series-item",
                span(
                  cls := "times-series-expression",
                  s"$n × $table",
                  span(cls := "times-series-answer", s"= ${n * table}")
                )
              )
            }.toList
          ),
          div(
            cls := "times-series-visuals",
            renderWaldorfFlower(table)
          )
        ): HtmlElement
      }
    )
  end renderSeries

  private def renderWaldorfFlower(table: Int): HtmlElement =
    import com.raquo.laminar.api.L.svg as S
    val center = 110.0
    val radius = 78.0
    val points = (0 until 10).map { n =>
      val angle = (-Math.PI / 2) + (2 * Math.PI * n.toDouble / 10.0)
      val x = center + radius * Math.cos(angle)
      val y = center + radius * Math.sin(angle)
      (x, y)
    }
    val trace = (0 to 10).map(n => (n * table) % 10)
    val lines = trace
      .sliding(2)
      .collect { case Seq(from, to) =>
        val (x1, y1) = points(from)
        val (x2, y2) = points(to)
        S.line(
          cls := "times-waldorf-line",
          S.x1 := f"$x1%.2f",
          S.y1 := f"$y1%.2f",
          S.x2 := f"$x2%.2f",
          S.y2 := f"$y2%.2f"
        )
      }
      .toList
    val nodes = points.zipWithIndex.map { case ((x, y), value) =>
      S.g(
        S.circle(
          cls := "times-waldorf-node",
          S.cx := f"$x%.2f",
          S.cy := f"$y%.2f",
          S.r := "12"
        ),
        S.text(
          cls := "times-waldorf-node-label",
          S.x := f"$x%.2f",
          S.y := f"${y + waldorfLabelYOffset}%.2f",
          value.toString
        )
      )
    }.toList
    div(
      cls := "times-visual-card",
      h4("Waldorf multiplication flower"),
      p(
        cls := "times-visual-caption",
        s"Trace $table around a circle of 10 to see the repeating pattern."
      ),
      S.svg(
        cls := "times-waldorf-svg",
        S.viewBox := "0 0 220 220",
        S.circle(
          cls := "times-waldorf-ring",
          S.cx := "110",
          S.cy := "110",
          S.r := "88"
        ),
        lines,
        nodes
      )
    )
  end renderWaldorfFlower

  private def renderRoughDotGroup(dotsInGroup: Int, groupIndex: Int): HtmlElement =
    import com.raquo.laminar.api.L.svg as S
    import org.scalajs.dom
    import roughjs.{Rough, RoughOptions}

    val dotDiameter = 16.0
    val gap = 8.0
    val padding = 10.0
    val rows = Math.ceil(dotsInGroup.toDouble / dotsPerRow).toInt.max(1)
    val width = (padding * 2) + (dotsPerRow * dotDiameter) + ((dotsPerRow - 1) * gap)
    val height = (padding * 2) + (rows * dotDiameter) + ((rows - 1) * gap)

    div(
      S.svg(
        cls := "times-rough-dots-svg",
        S.viewBox := f"0 0 $width%.2f $height%.2f",
        S.style := "width: 100%; height: auto; display: block;",
        onMountCallback { ctx =>
          val svgDom = ctx.thisNode.ref.asInstanceOf[dom.SVGSVGElement]
          svgDom.replaceChildren()
          val rough = Rough.svg(svgDom)
          (0 until dotsInGroup).foreach { dot =>
            val row = dot / dotsPerRow
            val col = dot % dotsPerRow
            val x = padding + (dotDiameter / 2) + col * (dotDiameter + gap)
            val y = padding + (dotDiameter / 2) + row * (dotDiameter + gap)
            val opts = new RoughOptions {}
            opts.fill = palette(groupIndex % palette.size)
            opts.stroke = "#334155"
            opts.strokeWidth = 1.4
            opts.roughness = roughDotRoughness
            opts.bowing = roughDotBowing
            opts.fillStyle = "solid"
            val dotEl = rough.circle(x, y, dotDiameter, opts)
            svgDom.appendChild(dotEl)
          }
        }
      )
    )
  end renderRoughDotGroup

  private def renderDots(groupCount: Int, groupSize: Int, title: String): HtmlElement =
    val total = groupCount * groupSize
    val tenGroups = (0 until Math.ceil(total.toDouble / dotsPerTenGroup).toInt).map { group =>
      val start = group * dotsPerTenGroup
      val dotsInGroup = Math.min(dotsPerTenGroup, total - start)
      div(
        cls := "times-ten-group",
        div(cls := "times-ten-group-label", s"Group #${group + 1}"),
        renderRoughDotGroup(dotsInGroup, group),
        div(cls := "times-ten-group-range", s"${start + 1}–${start + dotsInGroup}")
      )
    }
    div(
      cls := "times-visual-card",
      h4(title),
      p(
        cls := "times-visual-caption",
        s"$groupCount groups of $groupSize dots, chunked into groups of 10 for quick counting."
      ),
      div(cls := "times-ten-groups", tenGroups.toList),
      p(cls := "times-visual-total", s"$groupCount × $groupSize = $total")
    )
  end renderDots

  private def renderArray(question: Question): HtmlElement =
    div(
      cls := "times-visual-card",
      h4("Array view"),
      p(cls := "times-visual-caption", s"An array shows ${question.left} rows of ${question.right}."),
      div(
        cls := "times-array-visual",
        styleAttr := s"--times-cols: ${question.right};",
        (0 until question.product).map { idx =>
          span(
            cls := "times-array-cell",
            styleAttr := s"background: ${palette((idx / question.right) % palette.size)};"
          )
        }.toList
      )
    )
  end renderArray

  private def anchorHint(total: Int): String =
    if total > 10 then s"$total = 10 + ${total - 10}"
    else if total > 5 then s"$total = 5 + ${total - 5}"
    else s"$total is small enough to spot without counting each dot."
  end anchorHint

  private def renderFeedback(question: Question, submission: Submission): HtmlElement =
    submission.parsed match
      case None =>
        Callout(_.variant := "warning")(
          cls := "times-feedback-callout",
          "Please enter a whole-number answer."
        )
      case Some(answer) =>
        val correct = answer == question.product
        div(
          cls := "times-feedback-section",
          Callout(_.variant := (if correct then "success" else "warning"))(
            cls := "times-feedback-callout",
            if correct then s"Correct! ${question.prompt} = ${question.product}."
            else s"Nice try — ${question.prompt} = ${question.product}, not $answer."
          ),
          div(
            cls := "times-visuals",
            renderDots(question.left, question.right, s"${question.left} groups of ${question.right}"),
            renderDots(question.right, question.left, s"${question.right} groups of ${question.left}"),
            renderArray(question)
          ),
          Callout(_.variant := "neutral")(
            cls := "times-tip-callout",
            h4("Mental maths ideas"),
            ul(
              li(s"Turn-around fact: ${question.left} × ${question.right} = ${question.right} × ${question.left}."),
              li(s"Skip count in ${question.right}s: ${(1 to question.left).map(_ * question.right).mkString(", ")}."),
              li(s"Use an anchor number: ${anchorHint(question.product)}."),
              li("Spot doubles and near-doubles when one factor is 2, 4 or 8.")
            )
          )
        )
    end match
  end renderFeedback

  private def renderInteractive(mode: String): HtmlElement =
    div(
      cls := "times-interactive-section",
      child <-- questionVar.signal.combineWith(tableVar.signal).map { (question, table) =>
        div(
          Callout(_.variant := "neutral")(
            cls := "times-overview-callout",
            if mode == "quiz" then span(s"Quiz mode keeps you in the $table times table with one random fact at a time.")
            else span("Test mode mixes any question from the 1 to 12 times tables.")
          ),
          div(
            cls := "times-question-row",
            h3(cls := "times-question", s"${question.prompt} ="),
            input(
              typ := "number",
              cls := "times-answer-input times-answer-inline",
              placeholder := "?",
              aria.label := "Times table answer",
              value <-- answerVar.signal,
              onInput.mapToValue --> answerVar.writer
            )
          ),
          p(
            cls := "times-question-help",
            "Type your answer, check it, then use the visuals to see the same multiplication in different ways."
          ),
          div(
            cls := "times-question-actions",
            Button()(
              "Check answer",
              onClick --> (_ => checkAnswer())
            ),
            Button()(
              "New question",
              onClick --> (_ => resetQuestion(mode))
            )
          )
        ): HtmlElement
      },
      child <-- questionVar.signal.combineWith(submissionVar.signal).map {
        case (_, None)             => emptyNode
        case (question, Some(sub)) => renderFeedback(question, sub)
      }
    )
  end renderInteractive

  def render(): HtmlElement =
    modeVar.set("series")
    tableVar.set(2)
    answerVar.set("")
    questionVar.set(Question(2, 3))
    submissionVar.set(None)

    div(
      cls := "times-table-page",
      h2("Times Tables"),
      p(
        cls := "times-table-intro",
        "Practise tables up to 12 in three ways: read a full table, answer one table at a time, or mix everything together."
      ),
      div(
        cls := "times-table-controls",
        div(
          cls := "times-mode-buttons",
          modeButton("series", "Series mode"),
          modeButton("quiz", "Quiz mode"),
          modeButton("test", "Test mode")
        ),
        child <-- modeVar.signal.map { mode =>
          if mode == "test" then
            p(
              cls := "times-mode-note",
              "Mixed questions from all tables 1 to 12."
            ): HtmlElement
          else
            label(
              cls := "times-table-picker",
              span("Choose a table"),
              select(
                cls := "times-table-select",
                value <-- tableVar.signal.map(_.toString),
                onChange.mapToValue.map(_.toIntOption.getOrElse(2).max(1).min(12)) --> Observer[Int] { table =>
                  tableVar.set(table)
                  if modeVar.now() == "quiz" then resetQuestion("quiz")
                  end if
                },
                (1 to 12).map(n => option(value := n.toString, s"$n times table")).toList
              )
            ): HtmlElement
        }
      ),
      child <-- modeVar.signal.map {
        case "series" => renderSeries()
        case "quiz"   => renderInteractive("quiz")
        case _        => renderInteractive("test")
      }
    )
  end render

end TimesTablePage
