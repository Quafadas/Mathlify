package mathlify

import munit.FunSuite
import MathExpr.*
import ForwardDiff.*

class ForwardDiffSuite extends FunSuite:

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def assertDiff(
      result: Either[String, (Double, Double)],
      expectedVal: Double,
      expectedDeriv: Double,
      tol: Double = 1e-9
  ): Unit =
    result match
      case Right((v, d)) =>
        assertEqualsDouble(v, expectedVal, tol, s"value mismatch")
        assertEqualsDouble(d, expectedDeriv, tol, s"deriv mismatch")
      case Left(err) => fail(s"differentiate failed: $err")

  private def assertGrad(
      result: Either[String, DiffResult],
      expectedVal: Double,
      expectedPartials: Map[String, Double],
      tol: Double = 1e-9
  ): Unit =
    result match
      case Right(dr) =>
        assertEqualsDouble(dr.value, expectedVal, tol, "value mismatch")
        expectedPartials.foreach { case (name, expected) =>
          dr.partials.get(name) match
            case Some(actual) => assertEqualsDouble(actual, expected, tol, s"∂f/∂$name mismatch")
            case None         => fail(s"missing partial for $name")
        }
      case Left(err) => fail(s"gradient failed: $err")

  private def diffExpr(ascii: String, env: Map[String, Double], wrt: String): Either[String, (Double, Double)] =
    AsciiMath.translate(ascii) match
      case Right(expr) => differentiate(expr, env, wrt)
      case Left(err)   => fail(s"parse error: $err")

  private def gradExpr(ascii: String, env: Map[String, Double]): Either[String, DiffResult] =
    AsciiMath.translate(ascii) match
      case Right(expr) => gradient(expr, env)
      case Left(err)   => fail(s"parse error: $err")

  // ── 1. Basic polynomial derivatives ───────────────────────────────────────

  test("d/dx(x^2) at x=3 gives value=9, deriv=6") {
    assertDiff(diffExpr("x^2", Map("x" -> 3.0), "x"), 9.0, 6.0)
  }

  test("d/dx(x^2) at x=0 gives value=0, deriv=0") {
    assertDiff(diffExpr("x^2", Map("x" -> 0.0), "x"), 0.0, 0.0)
  }

  test("d/dx(x^3) at x=2 gives value=8, deriv=12") {
    assertDiff(diffExpr("x^3", Map("x" -> 2.0), "x"), 8.0, 12.0)
  }

  test("d/dx(3x + 5) at x=1 gives value=8, deriv=3") {
    assertDiff(diffExpr("3x + 5", Map("x" -> 1.0), "x"), 8.0, 3.0)
  }

  // ── 2. Transcendental functions ───────────────────────────────────────────

  test("d/dx(e^x) at x=0 gives value=1, deriv=1") {
    assertDiff(diffExpr("e^x", Map("x" -> 0.0), "x"), 1.0, 1.0)
  }

  test("d/dx(e^x) at x=1 gives value=e, deriv=e") {
    assertDiff(diffExpr("e^x", Map("x" -> 1.0), "x"), math.E, math.E)
  }

  test("d/dx(sin(x)) at x=0 gives value=0, deriv=1") {
    assertDiff(diffExpr("sin(x)", Map("x" -> 0.0), "x"), 0.0, 1.0)
  }

  test("d/dx(cos(x)) at x=0 gives value=1, deriv=0") {
    assertDiff(diffExpr("cos(x)", Map("x" -> 0.0), "x"), 1.0, 0.0)
  }

  test("d/dx(log(x)) at x=1 gives value=0, deriv=1") {
    assertDiff(diffExpr("log(x)", Map("x" -> 1.0), "x"), 0.0, 1.0)
  }

  test("d/dx(sqrt(x)) at x=4 gives value=2, deriv=0.25") {
    assertDiff(diffExpr("sqrt(x)", Map("x" -> 4.0), "x"), 2.0, 0.25)
  }

  // ── 3. Chain rule compositions ────────────────────────────────────────────

  test("d/dx(e^(x^2)) at x=1 gives value=e, deriv=2e") {
    assertDiff(diffExpr("e^(x^2)", Map("x" -> 1.0), "x"), math.E, 2.0 * math.E)
  }

  test("d/dx(sin(x^2)) at x=0 gives value=0, deriv=0") {
    assertDiff(diffExpr("sin(x^2)", Map("x" -> 0.0), "x"), 0.0, 0.0)
  }

  test("d/dx(sqrt(x^2 + 1)) at x=0 gives value=1, deriv=0") {
    assertDiff(diffExpr("sqrt(x^2 + 1)", Map("x" -> 0.0), "x"), 1.0, 0.0)
  }

  // ── 4. Partial derivatives (single-variable API) ─────────────────────────

  test("d/dx(x^2 * y + y^3) at x=2,y=3 gives value=39, deriv=12") {
    assertDiff(diffExpr("x^2 * y + y^3", Map("x" -> 2.0, "y" -> 3.0), "x"), 39.0, 12.0)
  }

  test("d/dy(x^2 * y + y^3) at x=2,y=3 gives value=39, deriv=31") {
    assertDiff(diffExpr("x^2 * y + y^3", Map("x" -> 2.0, "y" -> 3.0), "y"), 39.0, 31.0)
  }

  test("d/dx(x * y) at x=3,y=4 gives value=12, deriv=4") {
    assertDiff(diffExpr("x * y", Map("x" -> 3.0, "y" -> 4.0), "x"), 12.0, 4.0)
  }

  test("d/dy(x * y) at x=3,y=4 gives value=12, deriv=3") {
    assertDiff(diffExpr("x * y", Map("x" -> 3.0, "y" -> 4.0), "y"), 12.0, 3.0)
  }

  // ── 5. Division / quotient rule ──────────────────────────────────────────

  test("d/dx(x / y) at x=4,y=2 gives value=2, deriv=0.5") {
    assertDiff(diffExpr("x / y", Map("x" -> 4.0, "y" -> 2.0), "x"), 2.0, 0.5)
  }

  test("d/dy(x / y) at x=4,y=2 gives value=2, deriv=-1") {
    assertDiff(diffExpr("x / y", Map("x" -> 4.0, "y" -> 2.0), "y"), 2.0, -1.0)
  }

  // ── 6. Constants have zero derivative ─────────────────────────────────────

  test("d/dx(5) at x=1 gives value=5, deriv=0") {
    assertDiff(diffExpr("5", Map("x" -> 1.0), "x"), 5.0, 0.0)
  }

  test("d/dx(pi) at x=1 gives value=pi, deriv=0") {
    assertDiff(diffExpr("pi", Map("x" -> 1.0), "x"), math.Pi, 0.0)
  }

  // ── 7. Gradient: all partial derivatives in one pass ─────────────────────

  test("gradient of x^2 * y + y^3 at (2,3) gives all partials") {
    assertGrad(
      gradExpr("x^2 * y + y^3", Map("x" -> 2.0, "y" -> 3.0)),
      39.0,
      Map("x" -> 12.0, "y" -> 31.0)
    )
  }

  test("gradient of x * y at (3,4) gives all partials") {
    assertGrad(
      gradExpr("x * y", Map("x" -> 3.0, "y" -> 4.0)),
      12.0,
      Map("x" -> 4.0, "y" -> 3.0)
    )
  }

  test("gradient of x / y at (4,2) gives all partials") {
    assertGrad(
      gradExpr("x / y", Map("x" -> 4.0, "y" -> 2.0)),
      2.0,
      Map("x" -> 0.5, "y" -> -1.0)
    )
  }

  test("gradient of sin(x) * cos(y) at (0, 0) gives all partials") {
    assertGrad(
      gradExpr("sin(x) * cos(y)", Map("x" -> 0.0, "y" -> 0.0)),
      0.0,
      Map("x" -> 1.0, "y" -> 0.0)
    )
  }

  test("gradient of constant expression gives empty partials") {
    assertGrad(
      gradExpr("5", Map.empty),
      5.0,
      Map.empty
    )
  }

end ForwardDiffSuite
