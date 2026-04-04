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

end AutoDiffPageTest
