package mathlify

class HomePageTest extends PlaywrightTestBase:

  test("home page loads") {
    val response = page.navigate(baseUrl)
    assert(response.ok(), s"Expected 200, got ${response.status()}")
    page.waitForSelector("#app")
    assert(page.querySelector("#app") != null, "Expected #app element to exist")
  }
end HomePageTest
