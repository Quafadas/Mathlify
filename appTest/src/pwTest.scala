package mathlify

import com.microsoft.playwright.*
import com.sun.net.httpserver.{HttpServer, HttpExchange}
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

  private val contentTypes = Map(
    "html" -> "text/html",
    "js" -> "application/javascript",
    "css" -> "text/css",
    "map" -> "application/json"
  )

  protected def startServer(): Unit =
    val publishDir = Path.of(BuildInfo.publishDir)
    server = HttpServer.create(InetSocketAddress(0), 0)
    val port = server.getAddress.getPort
    baseUrl = s"http://localhost:$port"

    server.createContext(
      "/",
      (exchange: HttpExchange) =>
        val requestPath = exchange.getRequestURI.getPath.stripPrefix("/")
        val filePath = if requestPath.isEmpty then "index.html" else requestPath
        val resolved = publishDir.resolve(filePath).normalize()

        if !resolved.startsWith(publishDir) then
          exchange.sendResponseHeaders(403, -1)
          exchange.close()
        else if Files.exists(resolved) && !Files.isDirectory(resolved) then
          val bytes = Files.readAllBytes(resolved)
          val ext = filePath.split('.').lastOption.getOrElse("")
          val ct = contentTypes.getOrElse(ext, "application/octet-stream")
          exchange.getResponseHeaders.add("Content-Type", ct)
          exchange.sendResponseHeaders(200, bytes.length.toLong)
          exchange.getResponseBody.write(bytes)
          exchange.getResponseBody.close()
        else
          exchange.sendResponseHeaders(404, -1)
          exchange.close()
        end if
    )
    server.start()
  end startServer

  protected def stopServer(): Unit =
    if server != null then server.stop(0)
end PlaywrightTestBase
