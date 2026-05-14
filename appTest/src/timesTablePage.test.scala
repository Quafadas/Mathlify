package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class TimesTablePageTest extends PlaywrightTestBase:

  private def openTimesTablePage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Practise Tables")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Times Tables")).waitFor()
    page.waitForSelector(".times-table-page")
  end openTimesTablePage

  private def answerCurrentQuestion(): Unit =
    val text = page.locator(".times-question").textContent()
    val pattern = raw"(\d+)\s×\s(\d+)\s=\s\?".r
    val answer = text match
      case pattern(left, right) => left.toInt * right.toInt
      case _                    => fail(s"Could not parse question text \"$text\". Expected format: \"number × number = ?\"")
    page.locator(".times-answer-input").fill(answer.toString)
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Check answer")).click()
  end answerCurrentQuestion

  test("series mode shows the selected table up to 12") {
    openTimesTablePage()
    assertEquals(page.locator(".times-series-item").count(), 12)
    assert(page.locator(".times-series-list").textContent().contains("12 × 2"))
    assert(page.locator(".times-series-list").textContent().contains("= 24"))
  }

  test("quiz mode shows visuals after an answer is checked") {
    openTimesTablePage()
    page.locator(".times-mode-btn").filter(new Locator.FilterOptions().setHasText("Quiz mode")).click()
    page.waitForSelector(".times-question")

    answerCurrentQuestion()

    assert(page.locator(".times-feedback-callout").isVisible())
    assertEquals(page.locator(".times-visual-card").count(), 3)
    assert(page.locator(".times-tip-callout").textContent().contains("Turn-around fact"))
  }

  test("test mode mixes questions and still shows visual supports") {
    openTimesTablePage()
    page.locator(".times-mode-btn").filter(new Locator.FilterOptions().setHasText("Test mode")).click()
    page.waitForSelector(".times-question")
    assert(page.locator(".times-mode-note").textContent().contains("Mixed questions"))

    answerCurrentQuestion()

    assert(page.locator(".times-dot-grid").count() >= 2)
    assert(page.locator(".times-array-visual").isVisible())
  }

end TimesTablePageTest
