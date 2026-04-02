package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class QuadraticPageTest extends PlaywrightTestBase:

  private def openQuadraticPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Explore Proof")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Deriving the Quadratic Formula")).waitFor()
    page.waitForSelector(".proof-section")
  end openQuadraticPage

  private def coeffInput(n: Int): Locator =
    page.locator("wa-input.coeff-input").nth(n).locator("input")
  end coeffInput

  // ── Proof section ─────────────────────────────────────────────────────────

  test("proof section loads with 8 steps and the QED callout") {
    openQuadraticPage()
    assertEquals(page.locator(".proof-section .proof-step").count(), 8)
    assert(page.locator(".qed-callout").textContent().contains("This is the quadratic formula"))
  }

  // ── Solver: answer shown before workings ─────────────────────────────────

  test("answer callout appears before the step-by-step workings in the DOM") {
    openQuadraticPage()
    page.waitForSelector(".answer-callout")
    val answerBeforeSteps = page.evaluate(
      """() => {
        |  const answer = document.querySelector('.answer-callout');
        |  const steps  = document.querySelector('.solver-steps');
        |  // DOCUMENT_POSITION_FOLLOWING (4) means steps comes after answer
        |  return !!(answer.compareDocumentPosition(steps) & Node.DOCUMENT_POSITION_FOLLOWING);
        |}""".stripMargin
    )
    assert(answerBeforeSteps.asInstanceOf[Boolean], "Expected answer callout to precede solver steps in the DOM")
  }

  // ── Solver: default coefficients (a=1, b=−5, c=6) ────────────────────────

  test("solver shows two real roots for the default coefficients") {
    openQuadraticPage()
    page.waitForSelector(".answer-callout")
    val answerText = page.locator(".answer-callout").textContent()
    assert(answerText.contains("Answer:"), "Expected 'Answer:' label in the callout")
    assertEquals(page.locator(".solver-step").count(), 7)
  }

  // ── Solver: changing coefficients updates the answer ─────────────────────

  test("changing coefficients to produce a repeated root updates the answer") {
    openQuadraticPage()
    page.waitForSelector(".answer-callout")

    // a=1, b=−2, c=1  →  discriminant = 0, x = 1
    coeffInput(1).fill("-2")
    coeffInput(2).fill("1")

    page.waitForSelector(".answer-callout")
    val answerText = page.locator(".answer-callout").textContent()
    assert(answerText.contains("Answer:"))
    // One repeated root → 4 working steps
    assertEquals(page.locator(".solver-step").count(), 4)
  }

  test("changing coefficients to produce complex roots updates the answer") {
    openQuadraticPage()
    page.waitForSelector(".answer-callout")

    // a=1, b=0, c=1  →  discriminant = −4, complex roots
    coeffInput(1).fill("0")
    coeffInput(2).fill("1")

    page.waitForSelector(".answer-callout")
    val answerText = page.locator(".answer-callout").textContent()
    assert(answerText.contains("Answer:"))
    // Complex roots → 5 working steps
    assertEquals(page.locator(".solver-step").count(), 5)
  }

  // ── Solver: invalid input (a = 0) ────────────────────────────────────────

  test("setting a=0 shows a warning and hides the answer") {
    openQuadraticPage()
    page.waitForSelector(".answer-callout")

    coeffInput(0).fill("0")

    page.waitForSelector("wa-callout[variant='warning']")
    assert(page.locator("wa-callout[variant='warning']").textContent().contains("a"))
    assertEquals(page.locator(".answer-callout").count(), 0)
  }

end QuadraticPageTest
