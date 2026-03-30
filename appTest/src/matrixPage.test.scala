package mathlify

import com.microsoft.playwright.*

class MatrixPageTest extends PlaywrightTestBase:

  override def beforeAll(): Unit =
    pw = Playwright.create()
    browser = pw.firefox().launch(options);
    page = browser.newPage();
    page.setDefaultTimeout(30000)
  end beforeAll


  test("placeholder test") {

    assertEquals(1 + 1, 2)
  }