package mathlify

import com.microsoft.playwright.*
import scala.compiletime.uninitialized
import scala.collection.JavaConverters.seqAsJavaListConverter

trait PlaywrightTestBase extends munit.FunSuite:
  var pw: Playwright = uninitialized
  var browser: Browser = uninitialized
  var page: Page = uninitialized

  val options = new BrowserType.LaunchOptions()
    .setHeadless(true)
    .setArgs(List("--no-sandbox").asJava)