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
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.waitForSelector("#app")
    assert(page.querySelector("#app") != null, "Expected #app element to exist")
  }
end MatrixPageTest
