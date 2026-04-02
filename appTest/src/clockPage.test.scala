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

  test("clock page loads with SVG clock and default state") {
    openClockPage()
    assert(page.locator(".clock-svg-container svg").isVisible())
    // Default n=12, should have 12 number nodes (circles) and 12 labels
    assertEquals(page.locator(".clock-svg-container circle").count(), 13) // 12 nodes + 1 ring
    assertEquals(page.locator(".clock-svg-container text").count(), 12)
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
    openClockPage()
    // Default: a=3, b=5, add, n=12 → 3 + 5 mod 12 = 8
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    assert(text.contains("3 + 5 mod 12"), s"Expected expression in callout, got: $text")
    assert(text.contains("8"), s"Expected result 8 in callout, got: $text")
  }

  test("changing operation to multiplication updates the result callout") {
    openClockPage()
    // Click 'Multiplication' button
    page.locator(".clock-op-btn").filter(new Locator.FilterOptions().setHasText("Multiplication")).click()
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    // 3 × 5 mod 12 = 15 mod 12 = 3
    assert(text.contains("3 × 5 mod 12"), s"Expected mul expression, got: $text")
    assert(text.contains("3"), s"Expected result 3 in callout, got: $text")
  }

  test("switching to pattern view shows pattern callout instead of result") {
    openClockPage()
    page.locator(".clock-mode-btn").filter(new Locator.FilterOptions().setHasText("Pattern view")).click()
    assert(page.locator(".clock-pattern-callout").isVisible())
    assert(page.locator(".clock-result-callout").count() == 0)
  }

  test("sequence chips are shown in single mode") {
    openClockPage()
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
    openClockPage()
    page.locator(".clock-op-btn").filter(new Locator.FilterOptions().setHasText("Powers")).click()
    val callout = page.locator(".clock-result-callout")
    assert(callout.isVisible())
    val text = callout.textContent()
    // 3^5 mod 12 = 243 mod 12 = 3
    assert(text.contains("3^5 mod 12"), s"Expected pow expression, got: $text")
    assert(text.contains("3"), s"Expected result 3, got: $text")
  }

  test("animate mode shows step controls and builds pattern incrementally") {
    openClockPage()
    // Switch to animate mode
    page.locator(".clock-mode-btn").filter(new Locator.FilterOptions().setHasText("Animate")).click()
    // Animate controls should appear
    assert(page.locator(".clock-animate-controls").isVisible())
    // Initially 0 steps visible, step label shows "0 / 12 steps"
    val label = page.locator(".clock-anim-step-label")
    assert(label.textContent().contains("0 / 12"), s"Expected '0 / 12' step label, got: ${label.textContent()}")
    // Prev should be disabled, Next should be enabled
    assert(!page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).isDisabled())
    // Click Next a few times
    page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    assert(label.textContent().contains("2 / 12"), s"Expected '2 / 12' after two nexts, got: ${label.textContent()}")
    // Reset brings back to 0
    page.locator(".clock-anim-btn").filter(new Locator.FilterOptions().setHasText("Reset")).click()
    assert(label.textContent().contains("0 / 12"), s"Expected '0 / 12' after reset, got: ${label.textContent()}")
  }

  test("howto section is rendered below discover mode") {
    openClockPage()
    assert(page.locator(".clock-howto").isVisible())
    assert(page.locator(".clock-howto-section").count() == 3)
  }

end ClockPageTest
