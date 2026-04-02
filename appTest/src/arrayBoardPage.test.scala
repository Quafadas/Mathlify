package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class ArrayBoardPageTest extends PlaywrightTestBase:

  private def openArrayBoardPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Play Now")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Array Board Game")).waitFor()
    page.waitForSelector(".array-board-grid")
  end openArrayBoardPage

  test("array board page loads with a 10x10 grid and controls") {
    openArrayBoardPage()

    assertEquals(page.locator(".array-board-cell").count(), 100)
    assert(page.locator(".array-dim-input").count() >= 2)
    assert(page.locator(".array-board-target").textContent().contains("Challenge"))
  }

  test("clicking a valid cell places an array and shows it in the facts list") {
    openArrayBoardPage()

    // Click the top-left cell (row 0, col 0) — default 3×4 selection
    page.locator(".array-board-cell").first().click()
    page.waitForSelector(".array-facts-list")

    val factsText = page.locator(".array-facts-list").textContent()
    assert(factsText.contains("×"), s"Expected multiplication fact, got: $factsText")
  }

  test("score callout updates after placing an array") {
    openArrayBoardPage()

    page.locator(".array-board-cell").first().click()

    val scoreText = page.locator(".array-board-score").textContent()
    assert(scoreText.contains("Score:"), s"Expected score callout, got: $scoreText")
  }

  test("reset button clears all placed arrays") {
    openArrayBoardPage()

    page.locator(".array-board-cell").first().click()
    page.waitForSelector(".array-facts-list")

    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Reset Board")).click()

    assertEquals(page.locator(".array-facts-list").count(), 0)
    assert(
      page.locator(".array-board-score").textContent().contains("Hover over the grid"),
      "Expected neutral callout after reset"
    )
  }

  test("new challenge button resets board and changes target") {
    openArrayBoardPage()

    val initialTarget = page.locator(".array-board-target").textContent()
    page.locator(".array-board-cell").first().click()
    page.waitForSelector(".array-facts-list")

    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("New Challenge")).click()

    assertEquals(page.locator(".array-facts-list").count(), 0)
    assert(page.locator(".array-board-target").textContent().contains("Challenge"))
  }

end ArrayBoardPageTest
