package mathlify

import munit.FunSuite
import MathExpr.*
import ForwardDiff.*
import spire.math.Jet

class ForwardDiffSuite extends FunSuite:

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def assertJet(
      result: EvalResult[Jet[Double]],
      expectedVal: Double,
      expectedDeriv: Double,
      tol: Double = 1e-9
  ): Unit =
    result match
      case Numeric(j) =>
        assertEqualsDouble(j.real, expectedVal, tol, s"value mismatch")
        assertEqualsDouble(j.infinitesimal(0), expectedDeriv, tol, s"deriv mismatch")
      case other => fail(s"expected Numeric(Jet(...)) but got $other")

  private def diffExpr(ascii: String, env: Map[String, Double], wrt: String): EvalResult[Jet[Double]] =
    AsciiMath.translate(ascii) match
      case Right(expr) => differentiate(expr, env, wrt)
      case Left(err)   => fail(s"parse error: $err")

  // ── 1. Basic polynomial derivatives ───────────────────────────────────────

  test("d/dx(x^2) at x=3 gives value=9, deriv=6") {
    assertJet(diffExpr("x^2", Map("x" -> 3.0), "x"), 9.0, 6.0)
  }

  test("d/dx(x^2) at x=0 gives value=0, deriv=0") {
    assertJet(diffExpr("x^2", Map("x" -> 0.0), "x"), 0.0, 0.0)
  }

  test("d/dx(x^3) at x=2 gives value=8, deriv=12") {
    assertJet(diffExpr("x^3", Map("x" -> 2.0), "x"), 8.0, 12.0)
  }

  test("d/dx(3x + 5) at x=1 gives value=8, deriv=3") {
    assertJet(diffExpr("3x + 5", Map("x" -> 1.0), "x"), 8.0, 3.0)
  }

  // ── 2. Transcendental functions ───────────────────────────────────────────

  test("d/dx(e^x) at x=0 gives value=1, deriv=1") {
    assertJet(diffExpr("e^x", Map("x" -> 0.0), "x"), 1.0, 1.0)
  }

  test("d/dx(e^x) at x=1 gives value=e, deriv=e") {
    assertJet(diffExpr("e^x", Map("x" -> 1.0), "x"), math.E, math.E)
  }

  test("d/dx(sin(x)) at x=0 gives value=0, deriv=1") {
    assertJet(diffExpr("sin(x)", Map("x" -> 0.0), "x"), 0.0, 1.0)
  }

  test("d/dx(cos(x)) at x=0 gives value=1, deriv=0") {
    assertJet(diffExpr("cos(x)", Map("x" -> 0.0), "x"), 1.0, 0.0)
  }

  test("d/dx(log(x)) at x=1 gives value=0, deriv=1") {
    assertJet(diffExpr("log(x)", Map("x" -> 1.0), "x"), 0.0, 1.0)
  }

  test("d/dx(sqrt(x)) at x=4 gives value=2, deriv=0.25") {
    assertJet(diffExpr("sqrt(x)", Map("x" -> 4.0), "x"), 2.0, 0.25)
  }

  // ── 3. Chain rule compositions ────────────────────────────────────────────

  test("d/dx(e^(x^2)) at x=1 gives value=e, deriv=2e") {
    assertJet(diffExpr("e^(x^2)", Map("x" -> 1.0), "x"), math.E, 2.0 * math.E)
  }

  test("d/dx(sin(x^2)) at x=0 gives value=0, deriv=0") {
    assertJet(diffExpr("sin(x^2)", Map("x" -> 0.0), "x"), 0.0, 0.0)
  }

  test("d/dx(sqrt(x^2 + 1)) at x=0 gives value=1, deriv=0") {
    assertJet(diffExpr("sqrt(x^2 + 1)", Map("x" -> 0.0), "x"), 1.0, 0.0)
  }

  // ── 4. Partial derivatives (multi-variable) ──────────────────────────────

  test("d/dx(x^2 * y + y^3) at x=2,y=3 gives value=39, deriv=12") {
    assertJet(diffExpr("x^2 * y + y^3", Map("x" -> 2.0, "y" -> 3.0), "x"), 39.0, 12.0)
  }

  test("d/dy(x^2 * y + y^3) at x=2,y=3 gives value=39, deriv=31") {
    assertJet(diffExpr("x^2 * y + y^3", Map("x" -> 2.0, "y" -> 3.0), "y"), 39.0, 31.0)
  }

  test("d/dx(x * y) at x=3,y=4 gives value=12, deriv=4") {
    assertJet(diffExpr("x * y", Map("x" -> 3.0, "y" -> 4.0), "x"), 12.0, 4.0)
  }

  test("d/dy(x * y) at x=3,y=4 gives value=12, deriv=3") {
    assertJet(diffExpr("x * y", Map("x" -> 3.0, "y" -> 4.0), "y"), 12.0, 3.0)
  }

  // ── 5. Division / quotient rule ──────────────────────────────────────────

  test("d/dx(x / y) at x=4,y=2 gives value=2, deriv=0.5") {
    assertJet(diffExpr("x / y", Map("x" -> 4.0, "y" -> 2.0), "x"), 2.0, 0.5)
  }

  test("d/dy(x / y) at x=4,y=2 gives value=2, deriv=-1") {
    assertJet(diffExpr("x / y", Map("x" -> 4.0, "y" -> 2.0), "y"), 2.0, -1.0)
  }

  // ── 6. Constants have zero derivative ─────────────────────────────────────

  test("d/dx(5) at x=1 gives value=5, deriv=0") {
    assertJet(diffExpr("5", Map("x" -> 1.0), "x"), 5.0, 0.0)
  }

  test("d/dx(pi) at x=1 gives value=pi, deriv=0") {
    assertJet(diffExpr("pi", Map("x" -> 1.0), "x"), math.Pi, 0.0)
  }

end ForwardDiffSuite
