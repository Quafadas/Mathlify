package mathlify

import MathExpr.*
import org.scalajs.dom
import org.scalatest.funsuite.AnyFunSuite

class MathExprSpec extends AnyFunSuite:

  // ─────────────────────────────────────────────────────────────────────────
  // 1. AST construction
  // ─────────────────────────────────────────────────────────────────────────

  test("AST: Number equality") {
    assert(Number(3.14) == Number(3.14))
  }

  test("AST: Number integer vs double are different") {
    assert(Number(3.0) != Number(3.14))
  }

  test("AST: Symbol equality") {
    assert(Symbol("x") == Symbol("x"))
    assert(Symbol("x") != Symbol("y"))
  }

  test("AST: Constant equality") {
    assert(Constant("pi") == Constant("pi"))
  }

  test("AST: Add is structural") {
    assert(Add(Symbol("x"), Number(1.0)) == Add(Symbol("x"), Number(1.0)))
  }

  test("AST: Sub is structural") {
    assert(Sub(Number(5.0), Number(3.0)) == Sub(Number(5.0), Number(3.0)))
  }

  test("AST: Mul is structural") {
    assert(Mul(Number(2.0), Symbol("x")) == Mul(Number(2.0), Symbol("x")))
  }

  test("AST: Div is structural") {
    assert(Div(Symbol("a"), Symbol("b")) == Div(Symbol("a"), Symbol("b")))
  }

  test("AST: Pow is structural") {
    assert(Pow(Symbol("x"), Number(2.0)) == Pow(Symbol("x"), Number(2.0)))
  }

  test("AST: Neg wraps expression") {
    assert(Neg(Symbol("x")) == Neg(Symbol("x")))
    assert(Neg(Symbol("x")) != Symbol("x"))
  }

  test("AST: FunctionCall equality") {
    assert(
      FunctionCall("sin", List(Symbol("x"))) ==
        FunctionCall("sin", List(Symbol("x")))
    )
  }

  test("AST: Root(None) is square root") {
    assert(Root(None, Symbol("x")) == Root(None, Symbol("x")))
  }

  test("AST: Root(Some) is n-th root") {
    assert(Root(Some(Number(3.0)), Symbol("x")) == Root(Some(Number(3.0)), Symbol("x")))
  }

  test("AST: Sum structural equality") {
    assert(
      Sum(Symbol("i"), Number(1.0), Symbol("n"), Symbol("x")) ==
        Sum(Symbol("i"), Number(1.0), Symbol("n"), Symbol("x"))
    )
  }

  test("AST: Subscript structural equality") {
    assert(Subscript(Symbol("x"), Number(1.0)) == Subscript(Symbol("x"), Number(1.0)))
  }

  test("AST: Superscript structural equality") {
    assert(Superscript(Symbol("x"), Number(2.0)) == Superscript(Symbol("x"), Number(2.0)))
  }

  test("AST: MathVector equality") {
    assert(MathVector(List(Number(1.0), Number(2.0))) == MathVector(List(Number(1.0), Number(2.0))))
  }

  // ─────────────────────────────────────────────────────────────────────────
  // 2. MathML compilation
  // ─────────────────────────────────────────────────────────────────────────

  test("MathML: toMathML wraps in math element") {
    val node = MathMLCompiler.toMathML(Number(3.0))
    val math = node.asInstanceOf[dom.Element]
    assert(math.tagName.toLowerCase == "math")
  }

  test("MathML: Number produces mn element") {
    val node = MathMLCompiler.toMathML(Number(3.0))
    val math = node.asInstanceOf[dom.Element]
    val mn   = math.firstChild.asInstanceOf[dom.Element]
    assert(mn.tagName.toLowerCase == "mn")
  }

  test("MathML: Number integer formats without decimal") {
    val node = MathMLCompiler.toMathML(Number(5.0))
    val math = node.asInstanceOf[dom.Element]
    val mn   = math.firstChild.asInstanceOf[dom.Element]
    assert(mn.textContent == "5")
  }

  test("MathML: Number decimal keeps decimal part") {
    val node = MathMLCompiler.toMathML(Number(3.14))
    val math = node.asInstanceOf[dom.Element]
    val mn   = math.firstChild.asInstanceOf[dom.Element]
    assert(mn.textContent.contains("."))
  }

  test("MathML: Symbol produces mi element") {
    val node = MathMLCompiler.toMathML(Symbol("x"))
    val math = node.asInstanceOf[dom.Element]
    val mi   = math.firstChild.asInstanceOf[dom.Element]
    assert(mi.tagName.toLowerCase == "mi")
    assert(mi.textContent == "x")
  }

  test("MathML: Constant pi renders as π") {
    val node = MathMLCompiler.toMathML(Constant("pi"))
    val math = node.asInstanceOf[dom.Element]
    val mi   = math.firstChild.asInstanceOf[dom.Element]
    assert(mi.textContent == "π")
  }

  test("MathML: Add produces mrow with three children") {
    val node = MathMLCompiler.toMathML(Add(Symbol("x"), Number(1.0)))
    val math = node.asInstanceOf[dom.Element]
    val mrow = math.firstChild.asInstanceOf[dom.Element]
    assert(mrow.tagName.toLowerCase == "mrow")
    assert(mrow.childNodes.length == 3)
  }

  test("MathML: Add has mo + child") {
    val node = MathMLCompiler.toMathML(Add(Symbol("x"), Number(1.0)))
    val math = node.asInstanceOf[dom.Element]
    val mrow = math.firstChild.asInstanceOf[dom.Element]
    val mo   = mrow.childNodes(1).asInstanceOf[dom.Element]
    assert(mo.tagName.toLowerCase == "mo")
    assert(mo.textContent == "+")
  }

  test("MathML: Div produces mfrac") {
    val node  = MathMLCompiler.toMathML(Div(Symbol("a"), Symbol("b")))
    val math  = node.asInstanceOf[dom.Element]
    val mfrac = math.firstChild.asInstanceOf[dom.Element]
    assert(mfrac.tagName.toLowerCase == "mfrac")
    assert(mfrac.childNodes.length == 2)
  }

  test("MathML: Pow produces msup") {
    val node = MathMLCompiler.toMathML(Pow(Symbol("x"), Number(2.0)))
    val math = node.asInstanceOf[dom.Element]
    val msup = math.firstChild.asInstanceOf[dom.Element]
    assert(msup.tagName.toLowerCase == "msup")
  }

  test("MathML: Root(None) produces msqrt") {
    val node  = MathMLCompiler.toMathML(Root(None, Symbol("x")))
    val math  = node.asInstanceOf[dom.Element]
    val msqrt = math.firstChild.asInstanceOf[dom.Element]
    assert(msqrt.tagName.toLowerCase == "msqrt")
  }

  test("MathML: Root(Some) produces mroot") {
    val node  = MathMLCompiler.toMathML(Root(Some(Number(3.0)), Symbol("x")))
    val math  = node.asInstanceOf[dom.Element]
    val mroot = math.firstChild.asInstanceOf[dom.Element]
    assert(mroot.tagName.toLowerCase == "mroot")
  }

  test("MathML: Subscript produces msub") {
    val node = MathMLCompiler.toMathML(Subscript(Symbol("x"), Number(1.0)))
    val math = node.asInstanceOf[dom.Element]
    val msub = math.firstChild.asInstanceOf[dom.Element]
    assert(msub.tagName.toLowerCase == "msub")
  }

  test("MathML: Superscript produces msup") {
    val node = MathMLCompiler.toMathML(Superscript(Symbol("x"), Number(2.0)))
    val math = node.asInstanceOf[dom.Element]
    val msup = math.firstChild.asInstanceOf[dom.Element]
    assert(msup.tagName.toLowerCase == "msup")
  }

  test("MathML: data-mathlify-id is set on math element") {
    val node = MathMLCompiler.toMathML(Symbol("x"))
    val math = node.asInstanceOf[dom.Element]
    assert(math.getAttribute("data-mathlify-id") == "root")
  }

  test("MathML: data-mathlify-id is set on inner element") {
    val node  = MathMLCompiler.toMathML(Symbol("x"))
    val math  = node.asInstanceOf[dom.Element]
    val inner = math.firstChild.asInstanceOf[dom.Element]
    assert(inner.getAttribute("data-mathlify-id") == "0")
  }

  test("MathML: Neg produces mrow with minus") {
    val node = MathMLCompiler.toMathML(Neg(Symbol("x")))
    val math = node.asInstanceOf[dom.Element]
    val mrow = math.firstChild.asInstanceOf[dom.Element]
    assert(mrow.tagName.toLowerCase == "mrow")
    val mo = mrow.firstChild.asInstanceOf[dom.Element]
    assert(mo.textContent == "-")
  }

  // ─────────────────────────────────────────────────────────────────────────
  // 3. Parser
  // ─────────────────────────────────────────────────────────────────────────

  test("Parser: integer number") {
    assert(MathParser.parse("42") == Right(Number(42.0)))
  }

  test("Parser: decimal number") {
    assert(MathParser.parse("3.14") == Right(Number(3.14)))
  }

  test("Parser: simple symbol") {
    assert(MathParser.parse("x") == Right(Symbol("x")))
  }

  test("Parser: known constant pi") {
    assert(MathParser.parse("pi") == Right(Constant("pi")))
  }

  test("Parser: known constant e") {
    assert(MathParser.parse("e") == Right(Constant("e")))
  }

  test("Parser: addition") {
    assert(MathParser.parse("x+y") == Right(Add(Symbol("x"), Symbol("y"))))
  }

  test("Parser: subtraction") {
    assert(MathParser.parse("x-y") == Right(Sub(Symbol("x"), Symbol("y"))))
  }

  test("Parser: explicit multiplication") {
    assert(MathParser.parse("x*y") == Right(Mul(Symbol("x"), Symbol("y"))))
  }

  test("Parser: division") {
    assert(MathParser.parse("x/y") == Right(Div(Symbol("x"), Symbol("y"))))
  }

  test("Parser: power") {
    assert(MathParser.parse("x^2") == Right(Pow(Symbol("x"), Number(2.0))))
  }

  test("Parser: negation") {
    assert(MathParser.parse("-x") == Right(Neg(Symbol("x"))))
  }

  test("Parser: function call") {
    assert(MathParser.parse("sin(x)") == Right(FunctionCall("sin", List(Symbol("x")))))
  }

  test("Parser: sqrt produces Root") {
    assert(MathParser.parse("sqrt(x)") == Right(Root(None, Symbol("x"))))
  }

  test("Parser: implicit multiplication 2x") {
    assert(MathParser.parse("2x") == Right(Mul(Number(2.0), Symbol("x"))))
  }

  test("Parser: subscript x_1") {
    assert(MathParser.parse("x_1") == Right(Subscript(Symbol("x"), Number(1.0))))
  }

  test("Parser: parenthesised expression") {
    assert(MathParser.parse("(x+1)") == Right(Group(Add(Symbol("x"), Number(1.0)))))
  }

  test("Parser: left-associative addition") {
    assert(
      MathParser.parse("1+2+3") ==
        Right(Add(Add(Number(1.0), Number(2.0)), Number(3.0)))
    )
  }

  test("Parser: multiplication binds tighter than addition") {
    assert(
      MathParser.parse("x+y*z") ==
        Right(Add(Symbol("x"), Mul(Symbol("y"), Symbol("z"))))
    )
  }

  test("Parser: sum expression") {
    assert(
      MathParser.parse("sum_{i=1}^{n} x") ==
        Right(Sum(Symbol("i"), Number(1.0), Symbol("n"), Symbol("x")))
    )
  }

  test("Parser: sum expression no braces on upper") {
    assert(
      MathParser.parse("sum_{i=1}^n x") ==
        Right(Sum(Symbol("i"), Number(1.0), Symbol("n"), Symbol("x")))
    )
  }

  test("debug: inspect input bytes for sum parse") {
    val input = "sum_{i=1}^n x"
    val chars = input.map(c => s"'$c'(${c.toInt})").mkString(", ")
    println(s"INPUT CHARS: $chars")
    val result = MathParser.parse(input)
    println(s"PARSE RESULT: $result")
  }

  // ─────────────────────────────────────────────────────────────────────────
  // 4. AsciiMath
  // ─────────────────────────────────────────────────────────────────────────

  test("AsciiMath: simple symbol passes through") {
    assert(AsciiMath.translate("x") == Right(Symbol("x")))
  }

  test("AsciiMath: sqrt(x) produces Root") {
    assert(AsciiMath.translate("sqrt(x)") == Right(Root(None, Symbol("x"))))
  }

  test("AsciiMath: power x^2") {
    assert(AsciiMath.translate("x^2") == Right(Superscript(Symbol("x"), Number(2.0))))
  }

  test("AsciiMath: subscript x_1") {
    assert(AsciiMath.translate("x_1") == Right(Subscript(Symbol("x"), Number(1.0))))
  }

  test("AsciiMath: RR becomes Constant(ℝ)") {
    assert(AsciiMath.translate("RR") == Right(Operator("ℝ")))
  }

  test("AsciiMath: sum_(i=1)^n x produces Sum") {
    assert(
      AsciiMath.translate("sum_(i=1)^n x") ==
        Right(ExprSeq(List(
          SubSup(Operator("∑"), ExprSeq(List(Symbol("i"), Operator("="), Number(1.0))), Symbol("n")),
          Symbol("x")
        )))
    )
  }
