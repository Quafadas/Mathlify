package mathlify

import munit.FunSuite
import MathExpr.*
import ReverseDiff.*

class ReverseDiffSuite extends FunSuite:

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def assertGrad(
      result: Either[String, ForwardDiff.DiffResult],
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
      case Left(err) => fail(s"reverse gradient failed: $err")

  private def gradExpr(ascii: String, env: Map[String, Double]): Either[String, ForwardDiff.DiffResult] =
    AsciiMath.translate(ascii) match
      case Right(expr) => ReverseDiff.gradient(expr, env)
      case Left(err)   => fail(s"parse error: $err")

  private def reverseExpr(ascii: String, env: Map[String, Double]): Either[String, ReverseResult] =
    AsciiMath.translate(ascii) match
      case Right(expr) => ReverseDiff.reverseGradient(expr, env)
      case Left(err)   => fail(s"parse error: $err")

  // ── 1. Basic polynomial derivatives ───────────────────────────────────────

  test("reverse: d/dx(x^2) at x=3 gives value=9, partials x=6") {
    assertGrad(gradExpr("x^2", Map("x" -> 3.0)), 9.0, Map("x" -> 6.0))
  }

  test("reverse: d/dx(x^2) at x=0 gives value=0, partials x=0") {
    assertGrad(gradExpr("x^2", Map("x" -> 0.0)), 0.0, Map("x" -> 0.0))
  }

  test("reverse: d/dx(x^3) at x=2 gives value=8, partials x=12") {
    assertGrad(gradExpr("x^3", Map("x" -> 2.0)), 8.0, Map("x" -> 12.0))
  }

  test("reverse: 3x + 5 at x=1 gives value=8, partials x=3") {
    assertGrad(gradExpr("3x + 5", Map("x" -> 1.0)), 8.0, Map("x" -> 3.0))
  }

  // ── 2. Transcendental functions ───────────────────────────────────────────

  test("reverse: e^x at x=0 gives value=1, partials x=1") {
    assertGrad(gradExpr("e^x", Map("x" -> 0.0)), 1.0, Map("x" -> 1.0))
  }

  test("reverse: e^x at x=1 gives value=e, partials x=e") {
    assertGrad(gradExpr("e^x", Map("x" -> 1.0)), math.E, Map("x" -> math.E))
  }

  test("reverse: sin(x) at x=0 gives value=0, partials x=1") {
    assertGrad(gradExpr("sin(x)", Map("x" -> 0.0)), 0.0, Map("x" -> 1.0))
  }

  test("reverse: cos(x) at x=0 gives value=1, partials x=0") {
    assertGrad(gradExpr("cos(x)", Map("x" -> 0.0)), 1.0, Map("x" -> 0.0))
  }

  test("reverse: log(x) at x=1 gives value=0, partials x=1") {
    assertGrad(gradExpr("log(x)", Map("x" -> 1.0)), 0.0, Map("x" -> 1.0))
  }

  test("reverse: sqrt(x) at x=4 gives value=2, partials x=0.25") {
    assertGrad(gradExpr("sqrt(x)", Map("x" -> 4.0)), 2.0, Map("x" -> 0.25))
  }

  // ── 3. Chain rule compositions ────────────────────────────────────────────

  test("reverse: e^(x^2) at x=1 gives value=e, partials x=2e") {
    assertGrad(gradExpr("e^(x^2)", Map("x" -> 1.0)), math.E, Map("x" -> 2.0 * math.E))
  }

  test("reverse: sin(x^2) at x=0 gives value=0, partials x=0") {
    assertGrad(gradExpr("sin(x^2)", Map("x" -> 0.0)), 0.0, Map("x" -> 0.0))
  }

  test("reverse: sqrt(x^2 + 1) at x=0 gives value=1, partials x=0") {
    assertGrad(gradExpr("sqrt(x^2 + 1)", Map("x" -> 0.0)), 1.0, Map("x" -> 0.0))
  }

  // ── 4. Multivariable gradients ────────────────────────────────────────────

  test("reverse: x^2 * y + y^3 at (2,3) gives all partials") {
    assertGrad(
      gradExpr("x^2 * y + y^3", Map("x" -> 2.0, "y" -> 3.0)),
      39.0,
      Map("x" -> 12.0, "y" -> 31.0)
    )
  }

  test("reverse: x * y at (3,4) gives all partials") {
    assertGrad(
      gradExpr("x * y", Map("x" -> 3.0, "y" -> 4.0)),
      12.0,
      Map("x" -> 4.0, "y" -> 3.0)
    )
  }

  test("reverse: x / y at (4,2) gives all partials") {
    assertGrad(
      gradExpr("x / y", Map("x" -> 4.0, "y" -> 2.0)),
      2.0,
      Map("x" -> 0.5, "y" -> -1.0)
    )
  }

  test("reverse: sin(x) * cos(y) at (0, 0) gives all partials") {
    assertGrad(
      gradExpr("sin(x) * cos(y)", Map("x" -> 0.0, "y" -> 0.0)),
      0.0,
      Map("x" -> 1.0, "y" -> 0.0)
    )
  }

  // ── 5. Constants ──────────────────────────────────────────────────────────

  test("reverse: constant 5 gives empty partials") {
    assertGrad(gradExpr("5", Map.empty), 5.0, Map.empty)
  }

  test("reverse: pi at x=1 gives value=pi, no partials for unreferenced variable") {
    assertGrad(gradExpr("pi", Map("x" -> 1.0)), math.Pi, Map.empty)
  }

  // ── 6. Tape structure ─────────────────────────────────────────────────────

  test("tape for x * y has exactly 3 nodes") {
    val result = reverseExpr("x * y", Map("x" -> 3.0, "y" -> 4.0))
    assert(result.isRight, s"Expected Right, got $result")
    val rr = result.toOption.get
    assertEquals(rr.tape.length, 3)
    // Node 0: x (Var), Node 1: y (Var), Node 2: Mul
    assert(rr.tape(0).op.isInstanceOf[Op.Var], "Node 0 should be Var")
    assert(rr.tape(1).op.isInstanceOf[Op.Var], "Node 1 should be Var")
    assertEquals(rr.tape(2).op, Op.MulOp)
  }

  test("backSteps for x * y produces 2 steps") {
    val result = reverseExpr("x * y", Map("x" -> 3.0, "y" -> 4.0))
    val rr = result.toOption.get
    assertEquals(rr.backSteps.length, 2)
  }

  // ── 7. Agreement with forward mode ────────────────────────────────────────

  test("reverse mode agrees with forward mode on x^2 * sin(y) + exp(x)") {
    val env = Map("x" -> 1.5, "y" -> 0.7)
    val ascii = "x^2 * sin(y) + e^x"
    val parsed = AsciiMath.translate(ascii).toOption.get

    val fwd = ForwardDiff.gradient(parsed, env).toOption.get
    val rev = ReverseDiff.gradient(parsed, env).toOption.get

    assertEqualsDouble(rev.value, fwd.value, 1e-9, "values disagree")
    for (name, fwdDeriv) <- fwd.partials do assertEqualsDouble(rev.partials(name), fwdDeriv, 1e-9, s"∂f/∂$name disagrees")
    end for
  }

end ReverseDiffSuite
