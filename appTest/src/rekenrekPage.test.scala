package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class RekenrekPageTest extends PlaywrightTestBase:

  private def openRekenrekPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Start Counting")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Interactive Rekenrek")).waitFor()
    page.waitForSelector(".rekenrek-frame")
  end openRekenrekPage

  test("rekenrek page loads with two rows of beads and a total callout") {
    openRekenrekPage()

    assertEquals(page.locator(".rekenrek-row").count(), 2)
    // 10 beads per row but initially 0 active → all 10 are inactive per row
    assertEquals(page.locator(".bead-inactive-red").count() + page.locator(".bead-inactive-blue").count(), 20)
    // The neutral "start counting" callout should be visible
    assert(page.locator("wa-callout").last().textContent().contains("Click beads to start counting"))
  }

  test("clicking a bead activates it and all beads to its left") {
    openRekenrekPage()

    // Click the 5th inactive bead in row 1 (index 4 → sets count = 5)
    page.locator(".rekenrek-inactive-group").first().locator(".rekenrek-bead").nth(4).click()

    val row1Count = page.locator(".rekenrek-row").first().locator(".rekenrek-row-count")
    assertEquals(row1Count.textContent().trim, "5")
    assertEquals(page.locator(".bead-red").count(), 5)
  }

  test("clicking an active bead deactivates it and all beads to its right") {
    openRekenrekPage()

    // First activate 5 beads in row 1
    page.locator(".rekenrek-inactive-group").first().locator(".rekenrek-bead").nth(4).click()
    assertEquals(page.locator(".rekenrek-row").first().locator(".rekenrek-row-count").textContent().trim, "5")

    // Now click the 3rd active bead (index 2 → sets count = 2)
    page.locator(".rekenrek-active-group").first().locator(".rekenrek-bead").nth(2).click()
    assertEquals(page.locator(".rekenrek-row").first().locator(".rekenrek-row-count").textContent().trim, "2")
    assertEquals(page.locator(".bead-red").count(), 2)
  }

  test("total callout shows sum of both rows") {
    openRekenrekPage()

    // Activate 3 beads in row 1
    page.locator(".rekenrek-inactive-group").nth(0).locator(".rekenrek-bead").nth(2).click()
    // Activate 4 beads in row 2
    page.locator(".rekenrek-inactive-group").nth(1).locator(".rekenrek-bead").nth(3).click()

    val calloutText = page.locator(".rekenrek-total-callout").last().textContent()
    assert(calloutText.contains("3 + 4 = 7"), s"Expected '3 + 4 = 7' but got: $calloutText")
  }

  test("reset button clears all beads") {
    openRekenrekPage()

    // Activate some beads
    page.locator(".rekenrek-inactive-group").nth(0).locator(".rekenrek-bead").nth(4).click()
    page.locator(".rekenrek-inactive-group").nth(1).locator(".rekenrek-bead").nth(2).click()

    assertEquals(page.locator(".rekenrek-row").nth(0).locator(".rekenrek-row-count").textContent().trim, "5")

    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Reset")).click()

    assertEquals(page.locator(".rekenrek-row").nth(0).locator(".rekenrek-row-count").textContent().trim, "0")
    assertEquals(page.locator(".rekenrek-row").nth(1).locator(".rekenrek-row-count").textContent().trim, "0")
    assert(page.locator("wa-callout").last().textContent().contains("Click beads to start counting"))
  }

end RekenrekPageTest
