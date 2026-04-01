package mathlify

import com.microsoft.playwright.*
import com.microsoft.playwright.options.AriaRole

class MatrixPageTest extends PlaywrightTestBase:

  private def openMatrixPage(): Unit =
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.getByRole(AriaRole.BUTTON, new Page.GetByRoleOptions().setName("Try It")).click()
    page.getByRole(AriaRole.HEADING, new Page.GetByRoleOptions().setName("Interactive Matrix Multiplication")).waitFor()
    page.waitForSelector(".result-cell")
  end openMatrixPage

  private def editableCell(matrixIndex: Int, cellIndex: Int): Locator =
    page.locator(".matrix-wrapper").nth(matrixIndex).locator(".editable-cell").nth(cellIndex)
  end editableCell

  private def resultCell(cellIndex: Int): Locator =
    page.locator(".matrix-wrapper").nth(2).locator(".result-cell").nth(cellIndex)
  end resultCell

  test("matrix page shows the default multiplication result") {
    openMatrixPage()

    val dimCheck = page.locator(".dim-check")
    assert(dimCheck.textContent().contains("inner dimensions match (3 = 3)"))
    assert(dimCheck.textContent().contains("Result will be 2×2"))

    assertEquals(resultCell(0).textContent().trim, "58")
    assertEquals(resultCell(1).textContent().trim, "64")
    assertEquals(resultCell(2).textContent().trim, "139")
    assertEquals(resultCell(3).textContent().trim, "154")
  }

  test("clicking a result cell shows the dot product calculation") {
    openMatrixPage()

    resultCell(0).click()

    val detail = page.locator(".calc-detail")
    page.waitForSelector(".calc-detail wa-card")
    assert(detail.textContent().contains("C(1, 1) calculation"))
    assert(detail.textContent().contains("1 × 7 + 2 × 9 + 3 × 11 = 58"))
  }

  test("editing a matrix cell recomputes the result") {
    openMatrixPage()

    editableCell(0, 0).fill("2")

    assertEquals(resultCell(0).textContent().trim, "65")

    resultCell(0).click()
    val detail = page.locator(".calc-detail")
    assert(detail.textContent().contains("2 × 7 + 2 × 9 + 3 × 11 = 65"))
  }

  test("calc detail updates reactively when a cell is edited after selection") {
    openMatrixPage()

    resultCell(0).click()
    page.waitForSelector(".calc-detail wa-card")
    val detail = page.locator(".calc-detail")
    assert(detail.textContent().contains("1 × 7 + 2 × 9 + 3 × 11 = 58"))

    editableCell(0, 0).fill("2")

    assert(detail.textContent().contains("2 × 7 + 2 × 9 + 3 × 11 = 65"))
  }

  test("dimension mismatch hides the result grid and shows an error") {
    openMatrixPage()

    val matrixB = page.locator("wa-textarea").nth(1).locator("textarea")
    matrixB.fill("[(7,8),(9,10)]")

    val dimCheck = page.locator(".dim-check")
    assert(dimCheck.textContent().contains("inner dimensions don't match (3 ≠ 2)"))
    assert(page.locator(".result-cell").count() == 0)
    assert(page.locator("wa-callout").last().textContent().contains("Dimension mismatch: A is 2×3 but B is 2×2"))
  }



end MatrixPageTest
