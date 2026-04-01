package mathlify

import com.microsoft.playwright.*
import com.sun.net.httpserver.{SimpleFileServer, HttpServer}
import java.net.InetSocketAddress
import java.nio.file.{Files, Path}
import scala.compiletime.uninitialized
import scala.collection.JavaConverters.seqAsJavaListConverter

trait PlaywrightTestBase extends munit.FunSuite:
  var pw: Playwright = uninitialized
  var browser: Browser = uninitialized
  var page: Page = uninitialized
  var server: HttpServer = uninitialized
  var baseUrl: String = uninitialized

  val options = new BrowserType.LaunchOptions()
    .setHeadless(true)
    .setArgs(List("--no-sandbox").asJava)

  override def beforeAll(): Unit =
    startServer()
    pw = Playwright.create()
  end beforeAll

  override def afterAll(): Unit =
    if pw != null then pw.close()
    end if
    stopServer()
  end afterAll

  override def afterEach(context: AfterEach): Unit =
    if page != null then page.close()
    end if
    if browser != null then browser.close()
    end if
  end afterEach

  override def beforeEach(context: BeforeEach): Unit =
    browser = pw.chromium().launch(options)
    page = browser.newPage()
    page.setDefaultTimeout(10000)
  end beforeEach

  protected def startServer(): Unit =
    val publishDir = Path.of(BuildInfo.publishDir)
    val server = SimpleFileServer.createFileServer(InetSocketAddress(0), publishDir, SimpleFileServer.OutputLevel.NONE)
    val port = server.getAddress.getPort
    baseUrl = s"http://localhost:$port"
    println(s"Starting server at $baseUrl serving from $publishDir")
    server.start()
  end startServer

  protected def stopServer(): Unit =
    if server != null then server.stop(0)
end PlaywrightTestBase
