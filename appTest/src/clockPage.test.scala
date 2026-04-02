package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class ClockPageTest extends PlaywrightTestBase:

  private def openClockPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Explore Clocks")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Clock Arithmetic")).waitFor()
    page.waitForSelector(".clock-svg-container svg")
  end openClockPage

  private def openClockPageSingleStep(): Unit =
    openClockPage()
    page.locator(".clock-mode-btn").filter(new Locator.FilterOptions().setHasText("Single step")).click()
    page.waitForSelector(".clock-result-callout")
  end openClockPageSingleStep

  test("clock page loads with SVG clock and default state") {
    openClockPage()
    assert(page.locator(".clock-svg-container svg").isVisible())
    // Default n=10, should have 10 number nodes (circles) and 10 labels
    assertEquals(page.locator(".clock-svg-container circle").count(), 11) // 10 nodes + 1 ring
    assertEquals(page.locator(".clock-svg-container text").count(), 10)
  }

  test("modulus slider changes the number of positions") {
    openClockPage()
    // Change modulus to 6
    val slider = page.locator("input[type='range'].clock-slider")
    slider.fill("6")
    // Should now have 6 + 1 (ring) circles and 6 text nodes
    assertEquals(page.locator(".clock-svg-container circle").count(), 7)
    assertEquals(page.locator(".clock-svg-container text").count(), 6)
  }

  test("result callout shows correct addition result") {
    openClockPageSingleStep()
    // Default: a=1, b=2, add, n=10 → 1 + 2 mod 10 = 3
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    assert(text.contains("1 + 2 mod 10"), s"Expected expression in callout, got: $text")
    assert(text.contains("3"), s"Expected result 3 in callout, got: $text")
  }

  test("changing operation to multiplication updates the result callout") {
    openClockPageSingleStep()
    // Click 'Multiplication' button
    page.locator(".clock-op-btn").filter(new Locator.FilterOptions().setHasText("Multiplication")).click()
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    // 1 × 2 mod 10 = 2
    assert(text.contains("1 × 2 mod 10"), s"Expected mul expression, got: $text")
    assert(text.contains("2"), s"Expected result 2 in callout, got: $text")
  }

  test("switching to pattern view shows pattern callout instead of result") {
    openClockPage()
    page.locator(".clock-mode-btn").filter(new Locator.FilterOptions().setHasText("Pattern view")).click()
    assert(page.locator(".clock-pattern-callout").isVisible())
    assert(page.locator(".clock-result-callout").count() == 0)
  }

  test("sequence chips are shown in single mode") {
    openClockPageSingleStep()
    // default: a=3, b=5, add, n=12 - produces a sequence
    assert(page.locator(".clock-seq-row .seq-chip").count() > 0)
    // First chip has start styling
    assert(page.locator(".clock-seq-row .seq-chip-start").count() == 1)
    // Last chip has return styling
    assert(page.locator(".clock-seq-row .seq-chip-return").count() == 1)
  }

  test("discover mode cards are rendered and clickable") {
    openClockPage()
    val discoverCards = page.locator(".discover-card")
    assert(discoverCards.count() == 4)
    // Click the first discover card
    discoverCards.first().locator("wa-button").click()
    // After clicking, state should update - the clock SVG should still be visible
    assert(page.locator(".clock-svg-container svg").isVisible())
  }

  test("powers operation shows correct result") {
    openClockPageSingleStep()
    page.locator(".clock-op-btn").filter(new Locator.FilterOptions().setHasText("Powers")).click()
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    // 1^2 mod 10 = 1
    assert(text.contains("1^2 mod 10"), s"Expected pow expression, got: $text")
    assert(text.contains("1"), s"Expected result 1, got: $text")
  }

  test("animate mode shows step controls and builds pattern incrementally") {
    openClockPage()
    // Page opens in animate mode with animStepVar=1 (the visual hint default)
    // Default n=10, a=1, b=2, add → orbit [1,3,5,7,9,1] → maxStep=5
    val label = page.locator(".clock-anim-step-label")
    assert(label.textContent().contains("1 / 5"), s"Expected '1 / 5' as initial step, got: ${label.textContent()}")
    // At step 1, Prev is enabled and Next is enabled
    assert(!page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("← Prev")).isDisabled())
    assert(!page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).isDisabled())
    // Clicking Animate button resets step to 0
    page.locator(".clock-mode-btn").filter(new Locator.FilterOptions().setHasText("Animate")).click()
    assert(label.textContent().contains("0 / 5"), s"Expected '0 / 5' after re-clicking Animate, got: ${label.textContent()}")
    // Prev should be disabled at step 0
    assert(page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("← Prev")).isDisabled())
    // Click Next a few times
    page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    assert(label.textContent().contains("2 / 5"), s"Expected '2 / 5' after two nexts, got: ${label.textContent()}")
    // Reset brings back to 0
    page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Reset")).click()
    assert(label.textContent().contains("0 / 5"), s"Expected '0 / 5' after reset, got: ${label.textContent()}")
  }

  test("howto section is rendered below discover mode") {
    openClockPage()
    assert(page.locator(".clock-howto").isVisible())
    assert(page.locator(".clock-howto-section").count() == 3)
  }

  test("animate mode resets to step 0 when start value (a) changes") {
    openClockPage()
    // Page opens in animate mode (default). Set params — each change triggers a reset to 0.
    // Use n=9, multiplication, b=2 so orbit lengths differ by starting value:
    //   orbit from a=1: [1,2,4,8,7,5,1] → maxStep=6
    //   orbit from a=3: [3,6,3]          → maxStep=2
    page.locator("input.clock-number-input").nth(0).fill("1")
    page.locator("input.clock-number-input").nth(1).fill("2")
    page.locator(".clock-op-btn").filter(new Locator.FilterOptions().setHasText("Multiplication")).click()
    page.locator("input[type='range'].clock-slider").fill("9")
    val label = page.locator(".clock-anim-step-label")
    assert(label.textContent().contains("0 / 6"), s"Expected orbit length 6 from a=1, got: ${label.textContent()}")
    // Advance 3 steps
    for _ <- 1 to 3 do page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    end for
    assert(label.textContent().contains("3 / 6"), s"Expected step 3 / 6, got: ${label.textContent()}")
    // Change start value to 3 — animation must reset to step 0 with the new orbit length
    page.locator("input.clock-number-input").nth(0).fill("3")
    assert(label.textContent().contains("0 / 2"), s"Expected reset to 0 / 2 when a changes, got: ${label.textContent()}")
  }

  test("discover card updates page state correctly") {
    openClockPage()
    // First card: sets n=12, a=0, b=4, op=add, mode=single → result 0 + 4 mod 12 = 4
    page.locator(".discover-card").first().locator("wa-button").click()
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    assert(text.contains("0 + 4 mod 12"), s"Expected '0 + 4 mod 12' after discover card click, got: $text")
    assert(text.contains("4"), s"Expected result 4 in callout, got: $text")
  }

  test("changing input a updates the result callout") {
    openClockPageSingleStep()
    // Default a=1, b=2, add, n=10 → result 3
    val callout = page.locator(".clock-result-callout")
    assert(callout.textContent().contains("1 + 2 mod 10"))
    // Change a to 6: 6 + 2 mod 10 = 8
    page.locator("input.clock-number-input").nth(0).fill("6")
    val text = callout.textContent()
    assert(text.contains("6 + 2 mod 10"), s"Expected updated expression with a=6, got: $text")
    assert(text.contains("8"), s"Expected result 8, got: $text")
  }

  test("pattern view renders SVG path arrows") {
    openClockPage()
    page.locator(".clock-mode-btn").filter(new Locator.FilterOptions().setHasText("Pattern view")).click()
    // Each position maps to another, drawing at least one SVG path per arrow
    assert(page.locator(".clock-svg-container svg path").count() > 0)
  }

end ClockPageTest
