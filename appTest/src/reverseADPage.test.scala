package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class ReverseADPageTest extends PlaywrightTestBase:

  private def openReverseADPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Explore Backprop")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("What is Reverse-Mode Automatic Differentiation?")).waitFor()
  end openReverseADPage

  // ── Page structure ─────────────────────────────────────────────────────────

  test("page loads with theory, evaluator, and comparison sections") {
    openReverseADPage()
    assert(page.locator(".theory-section").count() == 1, "Expected theory section")
    assert(page.locator(".reverse-evaluator-section").count() == 1, "Expected evaluator section")
    assert(page.locator(".comparison-section").count() == 1, "Expected comparison section")
  }

  // ── Theory section ─────────────────────────────────────────────────────────

  test("theory section includes adjoint rules") {
    openReverseADPage()
    val rules = page.locator(".dual-rule")
    assert(rules.count() >= 6, s"Expected at least 6 adjoint rules, got ${rules.count()}")
  }

  // ── Interactive evaluator ──────────────────────────────────────────────────

  test("evaluator section shows expression input and variable controls") {
    openReverseADPage()
    val evaluator = page.locator(".reverse-evaluator-section")
    assert(evaluator.locator(".expression-card").count() == 1, "Expected expression card")
    assert(evaluator.locator(".variables-card").count() == 1, "Expected variables card")
    assert(evaluator.locator(".reverse-result-card").count() == 1, "Expected result card")
  }

  test("evaluator shows variable inputs for default expression") {
    openReverseADPage()
    val varInputs = page.locator(".variable-inputs")
    assert(varInputs.count() >= 1, "Expected variable inputs section")
  }

  // ── Computation graph ──────────────────────────────────────────────────────

  test("computation graph section is present") {
    openReverseADPage()
    assert(page.locator(".graph-section").count() == 1, "Expected graph section")
  }

  test("step controls are present when expression is evaluated") {
    openReverseADPage()
    page.getByLabel("x").fill("2")
    page.getByLabel("y").fill("3")
    page.locator(".derivative-result").waitFor()
    assert(page.locator(".step-controls").count() == 1, "Expected step controls")
  }

  test("tape table is present when expression is evaluated") {
    openReverseADPage()
    page.getByLabel("x").fill("2")
    page.getByLabel("y").fill("3")
    page.locator(".derivative-result").waitFor()
    assert(page.locator(".tape-table").count() == 1, "Expected tape table")
  }

  test("stepping through backward pass updates step counter") {
    openReverseADPage()
    page.getByLabel("x").fill("2")
    page.getByLabel("y").fill("3")
    page.locator(".derivative-result").waitFor()

    val stepCounter = page.locator(".step-counter")
    assert(stepCounter.textContent().contains("Step 0"), s"Expected step 0, got: ${stepCounter.textContent()}")

    // Click Next
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Next →")).click()
    assert(stepCounter.textContent().contains("Step 1"), s"Expected step 1 after clicking Next, got: ${stepCounter.textContent()}")
  }

  // ── Gradient results ──────────────────────────────────────────────────────

  test("gradient result shows correct partials for default expression") {
    openReverseADPage()
    page.getByLabel("x").fill("2")
    page.getByLabel("y").fill("3")
    page.locator(".derivative-result").waitFor()

    val resultText = page.locator(".derivative-result").textContent()
    // x^2 * y + y^3 at x=2, y=3: f=39, ∂f/∂x=12, ∂f/∂y=31
    assert(resultText.contains("39"), s"Expected f=39, got: $resultText")
    assert(resultText.contains("∂f/∂x"), s"Expected ∂f/∂x, got: $resultText")
    assert(resultText.contains("12"), s"Expected ∂f/∂x=12, got: $resultText")
    assert(resultText.contains("∂f/∂y"), s"Expected ∂f/∂y, got: $resultText")
    assert(resultText.contains("31"), s"Expected ∂f/∂y=31, got: $resultText")
  }

end ReverseADPageTest
