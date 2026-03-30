package mathlify

import com.microsoft.playwright.*

class MatrixPageTest extends PlaywrightTestBase:

  override def beforeAll(): Unit =
    startServer()
    pw = Playwright.create()
    browser = pw.firefox().launch(options)
    page = browser.newPage()
    page.setDefaultTimeout(30000)
  end beforeAll

  override def afterAll(): Unit =
    if page != null then page.close()
    end if
    if browser != null then browser.close()
    end if
    if pw != null then pw.close()
    end if
    stopServer()
  end afterAll

  test("home page loads") {
    page.navigate(baseUrl)
    val title = page.title()
    assert(title.nonEmpty, s"Expected a non-empty page title, got: '$title'")
  }
end MatrixPageTest
