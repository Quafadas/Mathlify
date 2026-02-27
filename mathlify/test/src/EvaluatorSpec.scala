package mathlify

import MathExpr.*
import Evaluator.*
import org.scalatest.funsuite.AnyFunSuite

class EvaluatorSpec extends AnyFunSuite:

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def assertNumeric(result: EvalResult, expected: Double, tol: Double = 1e-9): Unit =
    result match
      case Numeric(v) => assert(math.abs(v - expected) <= tol, s"expected $expected but got $v")
      case other      => fail(s"expected Numeric($expected) but got $other")

  private def assertError(result: EvalResult): Unit =
    assert(result.isInstanceOf[EvalError], s"expected EvalError but got $result")

  // ── 1. Free variable analysis ─────────────────────────────────────────────

  test("freeVars: Number has no free vars") {
    assert(freeVars(Number(3.0)) == Set.empty)
  }

  test("freeVars: Constant has no free vars") {
    assert(freeVars(Constant("pi")) == Set.empty)
  }

  test("freeVars: Symbol returns its name") {
    assert(freeVars(Symbol("x")) == Set("x"))
  }

  test("freeVars: Add collects from both sides") {
    assert(freeVars(Add(Symbol("x"), Symbol("y"))) == Set("x", "y"))
  }

  test("freeVars: nested expression collects all vars") {
    val expr = Add(Mul(Symbol("x"), Number(2.0)), Symbol("y"))
    assert(freeVars(expr) == Set("x", "y"))
  }

  // ── 2. isClosed ───────────────────────────────────────────────────────────

  test("isClosed: numeric literal is closed") {
    assert(isClosed(Number(5.0)))
  }

  test("isClosed: expression with variable is not closed") {
    assert(!isClosed(Add(Symbol("x"), Number(3.0))))
  }

  test("isClosed: constant pi is closed") {
    assert(isClosed(Constant("pi")))
  }

  // ── 3. unboundVars ────────────────────────────────────────────────────────

  test("unboundVars: closed expression has no unbound vars") {
    assert(unboundVars(Number(5.0)) == Set.empty)
  }

  test("unboundVars: single unbound symbol") {
    assert(unboundVars(Symbol("x")) == Set("x"))
  }

  test("unboundVars: all vars bound returns empty set") {
    assert(unboundVars(Add(Symbol("x"), Symbol("y")), Map("x" -> 1.0, "y" -> 2.0)) == Set.empty)
  }

  test("unboundVars: partially bound returns missing vars") {
    assert(unboundVars(Add(Symbol("x"), Symbol("y")), Map("x" -> 1.0)) == Set("y"))
  }

  test("unboundVars: nested expression with multiple missing vars") {
    val expr = Add(Mul(Symbol("x"), Symbol("y")), Symbol("z"))
    assert(unboundVars(expr, Map("x" -> 1.0)) == Set("y", "z"))
  }

  // ── 4. isEvaluable ────────────────────────────────────────────────────────

  test("isEvaluable: closed expression is always evaluable") {
    assert(isEvaluable(Number(42.0), Map.empty))
  }

  test("isEvaluable: x+3 evaluable when x is in env") {
    assert(isEvaluable(Add(Symbol("x"), Number(3.0)), Map("x" -> 2.0)))
  }

  test("isEvaluable: x+y not evaluable when y is missing") {
    assert(!isEvaluable(Add(Symbol("x"), Symbol("y")), Map("x" -> 2.0)))
  }

  // ── 5. Closed evaluation (no environment) ─────────────────────────────────

  test("eval: 2 + 3 = 5") {
    assertNumeric(eval(Add(Number(2.0), Number(3.0))), 5.0)
  }

  test("eval: 2 + 3 * 4 = 14") {
    val expr = Add(Number(2.0), Mul(Number(3.0), Number(4.0)))
    assertNumeric(eval(expr), 14.0)
  }

  test("eval: (2 + 3)^2 = 25") {
    val expr = Pow(Group(Add(Number(2.0), Number(3.0))), Number(2.0))
    assertNumeric(eval(expr), 25.0)
  }

  test("eval: sqrt(4) = 2") {
    val expr = Root(None, Number(4.0))
    assertNumeric(eval(expr), 2.0)
  }

  test("eval: sin(0) = 0") {
    val expr = FunctionCall("sin", List(Number(0.0)))
    assertNumeric(eval(expr), 0.0)
  }

  test("eval: cos(0) = 1") {
    val expr = FunctionCall("cos", List(Number(0.0)))
    assertNumeric(eval(expr), 1.0)
  }

  test("eval: exp(0) = 1") {
    val expr = FunctionCall("exp", List(Number(0.0)))
    assertNumeric(eval(expr), 1.0)
  }

  test("eval: pi constant evaluates to math.Pi") {
    assertNumeric(eval(Constant("pi")), math.Pi)
  }

  test("eval: e constant evaluates to math.E") {
    assertNumeric(eval(Constant("e")), math.E)
  }

  test("eval: negation of 5 = -5") {
    assertNumeric(eval(Neg(Number(5.0))), -5.0)
  }

  // ── 6. Environment evaluation ─────────────────────────────────────────────

  test("eval: x + 3 with x=2 gives 5") {
    val expr = Add(Symbol("x"), Number(3.0))
    assertNumeric(eval(expr, Map("x" -> 2.0)), 5.0)
  }

  test("eval: x^2 with x=5 gives 25") {
    val expr = Pow(Symbol("x"), Number(2.0))
    assertNumeric(eval(expr, Map("x" -> 5.0)), 25.0)
  }

  test("eval: x + y with x=3, y=4 gives 7") {
    val expr = Add(Symbol("x"), Symbol("y"))
    assertNumeric(eval(expr, Map("x" -> 3.0, "y" -> 4.0)), 7.0)
  }

  test("eval: x * y - z with all bound") {
    val expr = Sub(Mul(Symbol("x"), Symbol("y")), Symbol("z"))
    assertNumeric(eval(expr, Map("x" -> 3.0, "y" -> 4.0, "z" -> 2.0)), 10.0)
  }

  test("eval: log(x) with x=1 gives 0") {
    val expr = FunctionCall("log", List(Symbol("x")))
    assertNumeric(eval(expr, Map("x" -> 1.0)), 0.0)
  }

  // ── 7. Partial evaluation / constant folding ──────────────────────────────

  test("foldConstants: Add(2, 3) becomes Number(5)") {
    assert(foldConstants(Add(Number(2.0), Number(3.0))) == Number(5.0))
  }

  test("foldConstants: Mul(2, 3) becomes Number(6)") {
    assert(foldConstants(Mul(Number(2.0), Number(3.0))) == Number(6.0))
  }

  test("foldConstants: 2 + x + 3 stays unchanged (no reordering)") {
    val expr = Add(Add(Number(2.0), Symbol("x")), Number(3.0))
    val folded = foldConstants(expr)
    assert(folded == Add(Add(Number(2.0), Symbol("x")), Number(3.0)))
  }

  test("foldConstants: (2 * 3) + y folds to 6 + y") {
    val expr = Add(Mul(Number(2.0), Number(3.0)), Symbol("y"))
    val folded = foldConstants(expr)
    assert(folded == Add(Number(6.0), Symbol("y")))
  }

  test("foldConstants: x * (3 + 4) folds to x * 7") {
    val expr = Mul(Symbol("x"), Add(Number(3.0), Number(4.0)))
    val folded = foldConstants(expr)
    assert(folded == Mul(Symbol("x"), Number(7.0)))
  }

  test("partialEval: returns PartiallyReduced when vars are missing") {
    val expr = Add(Mul(Number(2.0), Number(3.0)), Symbol("y"))
    val result = partialEval(expr)
    result match
      case PartiallyReduced(e) => assert(e == Add(Number(6.0), Symbol("y")))
      case other               => fail(s"expected PartiallyReduced but got $other")
    end match
  }

  test("partialEval: returns Numeric when fully evaluable") {
    val expr = Add(Number(2.0), Number(3.0))
    assertNumeric(partialEval(expr), 5.0)
  }

  // ── 8. Error cases ────────────────────────────────────────────────────────

  test("eval: 1 / 0 returns EvalError") {
    assertError(eval(Div(Number(1.0), Number(0.0))))
  }

  test("eval: x + y with missing env entry returns EvalError") {
    assertError(eval(Add(Symbol("x"), Symbol("y")), Map("x" -> 1.0)))
  }

  test("eval: unsupported function returns EvalError") {
    val expr = FunctionCall("tanh", List(Number(0.0)))
    assertError(eval(expr))
  }

  test("eval: unbound variable with empty env returns EvalError") {
    assertError(eval(Symbol("z")))
  }

  test("eval: Fraction with zero denominator returns EvalError") {
    assertError(eval(Fraction(Number(1.0), Number(0.0))))
  }

  // ── 9. Additional arithmetic ──────────────────────────────────────────────

  test("eval: Sub(5, 3) = 2") {
    assertNumeric(eval(Sub(Number(5.0), Number(3.0))), 2.0)
  }

  test("eval: Div(10, 4) = 2.5") {
    assertNumeric(eval(Div(Number(10.0), Number(4.0))), 2.5)
  }

  test("foldConstants: Mul(0, x) becomes 0") {
    assert(foldConstants(Mul(Number(0.0), Symbol("x"))) == Number(0.0))
  }

  test("foldConstants: Mul(1, x) becomes x") {
    assert(foldConstants(Mul(Number(1.0), Symbol("x"))) == Symbol("x"))
  }

  test("eval: 3rd root of 8 = 2") {
    val expr = Root(Some(Number(3.0)), Number(8.0))
    assertNumeric(eval(expr), 2.0, tol = 1e-6)
  }

  // ── 10. AsciiMath integration ──────────────────────────────────────────────

  test("eval: AsciiMath 2*23 = 46") {
    AsciiMath.translate("2*23") match
      case Right(expr) => assertNumeric(eval(expr), 46.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("eval: AsciiMath 2+3 = 5") {
    AsciiMath.translate("2+3") match
      case Right(expr) => assertNumeric(eval(expr), 5.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("eval: AsciiMath 10-4 = 6") {
    AsciiMath.translate("10-4") match
      case Right(expr) => assertNumeric(eval(expr), 6.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("eval: AsciiMath 2+3*4 = 14 (precedence)") {
    AsciiMath.translate("2+3*4") match
      case Right(expr) => assertNumeric(eval(expr), 14.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("eval: AsciiMath (2+3)*4 = 20 (brackets)") {
    AsciiMath.translate("(2+3)*4") match
      case Right(expr) => assertNumeric(eval(expr), 20.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  // ── 10. Partial reduction of ExprSeq (issue: constants not folded) ─────────

  test("foldConstants: ExprSeq sqrt(x) + 2*3 + 1 reduces constant tail to 7") {
    val expr = ExprSeq(
      List(Root(None, Symbol("x")), Operator("+"), Number(2.0), Operator("*"), Number(3.0), Operator("+"), Number(1.0))
    )
    foldConstants(expr) match
      case ExprSeq(List(Root(None, Symbol("x")), Operator("+"), Number(7.0))) => ()
      case other                                                              => fail(s"expected ExprSeq([sqrt(x), +, 7]) but got $other")
    end match
  }

  test("partialEval: ExprSeq sqrt(x) + 2*3 + 1 returns PartiallyReduced with sqrt(x) + 7") {
    val expr = ExprSeq(
      List(Root(None, Symbol("x")), Operator("+"), Number(2.0), Operator("*"), Number(3.0), Operator("+"), Number(1.0))
    )
    partialEval(expr) match
      case PartiallyReduced(ExprSeq(List(Root(None, Symbol("x")), Operator("+"), Number(7.0)))) => ()
      case other                                                                                => fail(s"expected PartiallyReduced(ExprSeq([sqrt(x), +, 7])) but got $other")
    end match
  }

  test("partialEval: AsciiMath 'sqrt(x) + 2 * 3 + 1' partially reduces to sqrt(x) + 7") {
    AsciiMath.translate("sqrt(x) + 2 * 3 + 1") match
      case Right(expr) =>
        partialEval(expr) match
          case PartiallyReduced(ExprSeq(List(Root(None, Symbol("x")), Operator("+"), Number(7.0)))) => ()
          case other                                                                                => fail(s"expected PartiallyReduced(ExprSeq([sqrt(x), +, 7])) but got $other")
      case Left(err) => fail(s"parse error: $err")
  }

  test("partialEval: AsciiMath 'sqrt(x) + 2 * 3 + 1' with x=4 evaluates to 9") {
    AsciiMath.translate("sqrt(x) + 2 * 3 + 1") match
      case Right(expr) => assertNumeric(eval(expr, Map("x" -> 4.0)), 9.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("foldConstants: ExprSeq adjacent constant terms at end are combined") {
    val expr = ExprSeq(List(Symbol("x"), Operator("+"), Number(3.0), Operator("+"), Number(4.0)))
    foldConstants(expr) match
      case ExprSeq(List(Symbol("x"), Operator("+"), Number(7.0))) => ()
      case other                                                  => fail(s"expected ExprSeq([x, +, 7]) but got $other")
    end match
  }

  test("foldConstants: ExprSeq constant subtraction combined: x + 10 - 3 = x + 7") {
    val expr = ExprSeq(List(Symbol("x"), Operator("+"), Number(10.0), Operator("-"), Number(3.0)))
    foldConstants(expr) match
      case ExprSeq(List(Symbol("x"), Operator("+"), Number(7.0))) => ()
      case other                                                  => fail(s"expected ExprSeq([x, +, 7]) but got $other")
    end match
  }

  // ── 11. Partial reduction: distribute multiplication over addition ─────────

  test("foldConstants: 2 * (x + 3 + 4) reduces to Add(Mul(2, x), 14)") {
    val expr = Mul(Number(2.0), Group(Add(Add(Symbol("x"), Number(3.0)), Number(4.0))))
    val folded = foldConstants(expr)
    assert(folded == Add(Mul(Number(2.0), Symbol("x")), Number(14.0)))
  }

  test("partialEval: 2 * (x + 3 + 4) returns PartiallyReduced with 2*x + 14") {
    val expr = Mul(Number(2.0), Group(Add(Add(Symbol("x"), Number(3.0)), Number(4.0))))
    partialEval(expr) match
      case PartiallyReduced(Add(Mul(Number(2.0), Symbol("x")), Number(14.0))) => ()
      case other                                                              => fail(s"expected PartiallyReduced(2*x + 14) but got $other")
    end match
  }

  test("partialEval: MathParser 2*(x+3+4) reduces to 2*x + 14") {
    MathParser.parse("2*(x+3+4)") match
      case Right(expr) =>
        partialEval(expr) match
          case PartiallyReduced(Add(Mul(Number(2.0), Symbol("x")), Number(14.0))) => ()
          case other                                                              => fail(s"expected PartiallyReduced(2*x + 14) but got $other")
      case Left(err) => fail(s"parse error: $err")
  }

  test("foldConstants: distribute Number * Add without Group: 3 * (x + 2) = 3*x + 6") {
    val expr = Mul(Number(3.0), Add(Symbol("x"), Number(2.0)))
    assert(foldConstants(expr) == Add(Mul(Number(3.0), Symbol("x")), Number(6.0)))
  }

  test("foldConstants: (x + 3) + 4 collects constants to x + 7") {
    val expr = Add(Add(Symbol("x"), Number(3.0)), Number(4.0))
    assert(foldConstants(expr) == Add(Symbol("x"), Number(7.0)))
  }

  // ── 12. Superscript evaluation (AsciiMath produces Superscript for x^2) ────

  test("eval: AsciiMath x^2 with x=4 gives 16") {
    AsciiMath.translate("x^2") match
      case Right(expr) => assertNumeric(eval(expr, Map("x" -> 4.0)), 16.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("eval: AsciiMath x^2 + 5x with x=4 gives 36") {
    AsciiMath.translate("x^2 + 5x") match
      case Right(expr) => assertNumeric(eval(expr, Map("x" -> 4.0)), 36.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  test("eval: AsciiMath x^2 + 5x with x=0 gives 0") {
    AsciiMath.translate("x^2 + 5x") match
      case Right(expr) => assertNumeric(eval(expr, Map("x" -> 0.0)), 0.0)
      case Left(err)   => fail(s"parse error: $err")
  }

  // ── 13. Free variables: function application pattern ─────────────────────

  test("freeVars: f(x) in ExprSeq does not count f as free variable") {
    // AsciiMath parses f(x) as ExprSeq([Symbol("f"), BracketGroup("(",")",Symbol("x"))])
    val expr = ExprSeq(List(Symbol("f"), BracketGroup("(", ")", Symbol("x"))))
    assert(freeVars(expr) == Set("x"))
  }

  test("freeVars: AsciiMath f(x) does not count f as free variable") {
    AsciiMath.translate("f(x)") match
      case Right(expr) => assert(freeVars(expr) == Set("x"))
      case Left(err)   => fail(s"parse error: $err")
  }

  test("freeVars: f(x) = x^2 + 5x only has x as free variable, not f") {
    AsciiMath.translate("f(x) = x^2 + 5x") match
      case Right(expr) => assert(freeVars(expr) == Set("x"))
      case Left(err)   => fail(s"parse error: $err")
  }

  test("freeVars: f(x, y) = x^2 + 5x * y has x and y as free variables") {
    AsciiMath.translate("f(x, y) = x^2 + 5x * y") match
      case Right(expr) => assert(freeVars(expr) == Set("x", "y"))
      case Left(err)   => fail(s"parse error: $err")
  }
end EvaluatorSpec
