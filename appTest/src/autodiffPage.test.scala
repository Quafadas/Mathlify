package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class AutoDiffPageTest extends PlaywrightTestBase:

  private def openAutoDiffPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Explore Derivatives")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("What is Forward-Mode Automatic Differentiation?")).waitFor()
  end openAutoDiffPage

  // ── Page structure ─────────────────────────────────────────────────────────

  test("page loads with theory, examples, and evaluator sections") {
    openAutoDiffPage()
    assert(page.locator(".theory-section").count() == 1, "Expected theory section")
    assert(page.locator(".examples-section").count() == 1, "Expected examples section")
    assert(page.locator(".evaluator-section").count() == 1, "Expected evaluator section")
  }

  // ── Theory section ─────────────────────────────────────────────────────────

  test("theory section includes chain rule and quotient rule subsections") {
    openAutoDiffPage()
    assert(page.locator(".chain-rule-section").count() == 1, "Expected chain rule section")
    assert(page.locator(".quotient-rule-section").count() == 1, "Expected quotient rule section")
  }

  test("dual number arithmetic rules are displayed") {
    openAutoDiffPage()
    val rules = page.locator(".dual-rule")
    assert(rules.count() >= 4, s"Expected at least 4 dual number rules, got ${rules.count()}")
  }

  // ── Worked examples section ────────────────────────────────────────────────

  test("worked examples section shows at least 5 examples") {
    openAutoDiffPage()
    val examples = page.locator(".worked-example")
    assert(examples.count() >= 5, s"Expected at least 5 worked examples, got ${examples.count()}")
  }

  test("worked example for x^2 shows correct derivative") {
    openAutoDiffPage()
    val firstExample = page.locator(".worked-example").first()
    val text = firstExample.textContent()
    assert(text.contains("9"), s"Expected value 9 in first example, got: $text")
    assert(text.contains("6"), s"Expected derivative 6 in first example, got: $text")
  }

  // ── Interactive evaluator ──────────────────────────────────────────────────

  test("evaluator section shows expression input and variable controls") {
    openAutoDiffPage()
    val evaluator = page.locator(".evaluator-section")
    assert(evaluator.locator(".expression-card").count() == 1, "Expected expression card")
    assert(evaluator.locator(".variables-card").count() == 1, "Expected variables card")
    assert(evaluator.locator(".eval-card").count() == 1, "Expected eval card")
  }

  test("evaluator shows variable inputs for default expression") {
    openAutoDiffPage()
    val varInputs = page.locator(".variable-inputs")
    assert(varInputs.count() >= 1, s"Expected variable inputs section")
  }

  // ── Stale variable regression ──────────────────────────────────────────────

  test("removing a variable from expression removes its partial from results") {
    openAutoDiffPage()

    // Fill in both variables for the default expression x^2 * y + y^3
    page.getByLabel("x").fill("2")
    page.getByLabel("y").fill("2")
    // Wait for results to appear
    page.locator(".derivative-result").waitFor()

    // Now remove y from the expression, leaving just x^2
    val textarea = page.locator("wa-textarea").locator("textarea").first()
    textarea.fill("x^2")

    // Wait for the result to update
    page.locator(".derivative-result").waitFor()

    val resultText = page.locator(".derivative-result").textContent()
    assert(!resultText.contains("∂f/∂y"), s"Expected no ∂f/∂y after removing y, but got: $resultText")
    assert(resultText.contains("∂f/∂x"), s"Expected ∂f/∂x to still be present, got: $resultText")
  }

  test("adding a variable shows previous variable values in inputs") {
    openAutoDiffPage()

    val textarea = page.locator("wa-textarea").locator("textarea").first()

    // Start with x^2, set x=3
    textarea.fill("x^2")
    page.getByLabel("x").fill("3")
    page.locator(".derivative-result").waitFor()

    // Change to x^2 * y — x should still show 3 in the input
    textarea.fill("x^2 * y")
    page.getByLabel("y").waitFor()

    val xInput = page.getByLabel("x")
    val xValue = xInput.inputValue()
    assert(xValue == "3", s"Expected x input to still show 3 after adding y, but got: '$xValue'")

    // Fill y and verify result uses the retained x value
    page.getByLabel("y").fill("2")
    page.locator(".derivative-result").waitFor()

    val resultText = page.locator(".derivative-result").textContent()
    // x^2*y at x=3, y=2: f=18, ∂f/∂x=12, ∂f/∂y=9
    assert(resultText.contains("18"), s"Expected f=18, got: $resultText")
    assert(resultText.contains("12"), s"Expected ∂f/∂x=12, got: $resultText")
  }

end AutoDiffPageTest
