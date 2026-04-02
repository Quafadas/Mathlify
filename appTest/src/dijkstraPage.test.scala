package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class DijkstraPageTest extends PlaywrightTestBase:

  private def openDijkstraPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Explore Algorithm")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Dijkstra's Shortest-Path Algorithm")).waitFor()
    page.waitForSelector(".dijkstra-svg-container svg")
  end openDijkstraPage

  test("dijkstra page loads with SVG graph and initial state") {
    openDijkstraPage()
    assert(page.locator(".dijkstra-svg-container svg").isVisible())
    // 6 nodes → 6 circles
    assertEquals(page.locator(".dijkstra-svg-container circle").count(), 6)
    // Step counter shows "Initial State" at step 0
    val counter = page.locator(".dijkstra-step-counter")
    assert(counter.textContent().contains("Initial State"), s"Expected 'Initial State', got: ${counter.textContent()}")
  }

  test("initial distance table shows S=0 and all others as infinity") {
    openDijkstraPage()
    val rows = page.locator(".dijkstra-dist-table tbody tr")
    assertEquals(rows.count(), 6)
    // First row (S) should have distance 0
    val firstRow = rows.nth(0)
    assert(firstRow.textContent().contains("0"), s"Expected S=0, got: ${firstRow.textContent()}")
    // Others should show ∞
    val secondRow = rows.nth(1)
    assert(secondRow.textContent().contains("∞"), s"Expected A=∞, got: ${secondRow.textContent()}")
  }

  test("next button advances to step 1 and updates description") {
    openDijkstraPage()
    page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    val counter = page.locator(".dijkstra-step-counter")
    assert(counter.textContent().contains("1 / 6"), s"Expected '1 / 6', got: ${counter.textContent()}")
    val desc = page.locator(".dijkstra-step-desc")
    val text = desc.textContent()
    assert(text.contains("Visit S"), s"Expected description mentioning 'Visit S', got: $text")
  }

  test("after step 1 distance table updates for A and D") {
    openDijkstraPage()
    page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    val rows = page.locator(".dijkstra-dist-table tbody tr")
    // A (row 1) should now show 7
    val aRow = rows.nth(1)
    assert(aRow.textContent().contains("7"), s"Expected A=7, got: ${aRow.textContent()}")
    // D (row 4) should now show 2
    val dRow = rows.nth(4)
    assert(dRow.textContent().contains("2"), s"Expected D=2, got: ${dRow.textContent()}")
  }

  test("prev button is disabled on first step") {
    openDijkstraPage()
    assert(
      page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("← Prev")).isDisabled(),
      "Prev button should be disabled at step 0"
    )
  }

  test("next button is disabled on last step") {
    openDijkstraPage()
    // Advance to the last step (step 6)
    for _ <- 1 to 6 do page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    end for
    assert(
      page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).isDisabled(),
      "Next button should be disabled at last step"
    )
    val counter = page.locator(".dijkstra-step-counter")
    assert(counter.textContent().contains("6 / 6"), s"Expected '6 / 6', got: ${counter.textContent()}")
  }

  test("reset button returns to step 0") {
    openDijkstraPage()
    page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Reset")).click()
    val counter = page.locator(".dijkstra-step-counter")
    assert(counter.textContent().contains("Initial State"), s"Expected 'Initial State' after reset, got: ${counter.textContent()}")
  }

  test("final step description mentions algorithm complete") {
    openDijkstraPage()
    for _ <- 1 to 6 do page.locator(".dijkstra-ctrl-btn").filter(new Locator.FilterOptions().setHasText("Next →")).click()
    end for
    val desc = page.locator(".dijkstra-step-desc")
    val text = desc.textContent()
    assert(text.contains("Algorithm complete!"), s"Expected 'Algorithm complete!' in final step desc, got: $text")
  }

  test("legend is visible") {
    openDijkstraPage()
    assert(page.locator(".dijkstra-legend").isVisible())
  }

end DijkstraPageTest
