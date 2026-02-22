package mathlify

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import org.scalajs.dom.{document, html}
import scala.scalajs.js

/** Tests that [[MathComponent.render]] produces the expected DOM structure.
  *
  * Each test mounts a Laminar element into a fresh `<div>`, queries the DOM,
  * and then unmounts.  Requires a jsdom environment.
  *
  * To enable DOM tests, configure `JSDOMNodeJSEnv` in `build.mill`:
  *   1. Add `ivy"org.scala-js::scalajs-env-jsdom-nodejs:1.1.1"` to `mill-build/build.mill`
  *   2. In the test module add: `def jsEnv = new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()`
  */
class MathComponentSuite extends munit.FunSuite:

  /** True when a real document object is available (i.e. jsdom is loaded). */
  private def hasDocument: Boolean =
    !js.isUndefined(js.Dynamic.global.document)

  private def assumeDom(): Unit =
    assume(hasDocument, "Skipping DOM test: jsdom not available (configure JSDOMNodeJSEnv)")

  // ── helpers ──────────────────────────────────────────────────────────────

  private def mount(expr: MathExpr): (html.Div, DynamicSubscription) =
    val container = document.createElement("div").asInstanceOf[html.Div]
    document.body.appendChild(container)
    val sub = render(container, MathComponent.render(expr))
    (container, sub)

  private def cleanup(container: html.Div, sub: DynamicSubscription): Unit =
    sub.kill()
    document.body.removeChild(container)

  // ── atoms ────────────────────────────────────────────────────────────────

  test("renders a variable with class math-var"):
    assumeDom()
    val (c, sub) = mount(MathExpr.Var("x"))
    val el       = c.querySelector(".math-var")
    assert(el != null, ".math-var element should exist")
    assertEquals(el.textContent, "x")
    cleanup(c, sub)

  test("renders a number with class math-num"):
    assumeDom()
    val (c, sub) = mount(MathExpr.Num("42"))
    val el       = c.querySelector(".math-num")
    assert(el != null, ".math-num element should exist")
    assertEquals(el.textContent, "42")
    cleanup(c, sub)

  // ── operators ────────────────────────────────────────────────────────────

  test("renders addition: two math-var children and a math-plus operator"):
    assumeDom()
    val (c, sub) = mount(MathExpr.Add(MathExpr.Var("a"), MathExpr.Var("b")))
    assert(c.querySelector(".math-add") != null, ".math-add should exist")
    assertEquals(c.querySelectorAll(".math-var").length, 2)
    val op = c.querySelector(".math-plus")
    assert(op != null, ".math-plus should exist")
    assertEquals(op.textContent.trim, "+")
    cleanup(c, sub)

  test("renders subtraction with class math-minus"):
    assumeDom()
    val (c, sub) = mount(MathExpr.Sub(MathExpr.Var("a"), MathExpr.Var("b")))
    assert(c.querySelector(".math-sub") != null)
    assert(c.querySelector(".math-minus") != null)
    cleanup(c, sub)

  // ── parentheses ──────────────────────────────────────────────────────────

  test("renders parentheses with open/close paren spans"):
    assumeDom()
    val (c, sub) = mount(MathExpr.Parens(MathExpr.Var("a")))
    assert(c.querySelector(".math-parens") != null, ".math-parens should exist")
    assertEquals(c.querySelector(".math-paren-open").textContent, "(")
    assertEquals(c.querySelector(".math-paren-close").textContent, ")")
    cleanup(c, sub)

  // ── equation ─────────────────────────────────────────────────────────────

  test("renders an equation with class math-equation and math-equals operator"):
    assumeDom()
    val (c, sub) = mount(MathExpr.Eq(MathExpr.Var("a"), MathExpr.Var("b")))
    assert(c.querySelector(".math-equation") != null, ".math-equation should exist")
    val eq = c.querySelector(".math-equals")
    assert(eq != null, ".math-equals should exist")
    assertEquals(eq.textContent.trim, "=")
    cleanup(c, sub)

  // ── end-to-end ───────────────────────────────────────────────────────────

  test("renders the flagship associativity example and counts variables"):
    assumeDom()
    import mathlify.*
    val expr     = mathlify"(a + b) + c = a + (b + c)"
    val (c, sub) = mount(expr)

    // Top-level equation wrapper
    assert(c.querySelector(".math-equation") != null, ".math-equation should be present")

    // (a + b) + c = a + (b + c)  => 6 variable occurrences
    assertEquals(c.querySelectorAll(".math-var").length, 6)

    // Two pairs of parentheses
    assertEquals(c.querySelectorAll(".math-parens").length, 2)

    cleanup(c, sub)
