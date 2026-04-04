package mathlify

import munit.FunSuite
import MathExpr.*

class MathMLSerializerSuite extends FunSuite:

  // ── helpers ──────────────────────────────────────────────────────────────────

  private def serialize[A](expr: MathExpr[A])(using MathShow[A]): String =
    MathMLSerializer.toMathML(expr)

  private def assertContains(xml: String, fragment: String): Unit =
    assert(xml.contains(fragment), s"Expected '$fragment' in: $xml")

  private def assertTagExists(xml: String, tag: String): Unit =
    assertContains(xml, s"<$tag ")

  // ── wrapper ───────────────────────────────────────────────────────────────────

  test("toMathML wraps in math element") {
    val s = serialize(Number(3.0))
    assertContains(s, "<math ")
    assertContains(s, "http://www.w3.org/1998/Math/MathML")
  }

  test("toMathML sets root id") {
    val s = serialize(Number(1.0))
    assertContains(s, """data-mathlify-id="root"""")
  }

  // ── Number ────────────────────────────────────────────────────────────────────

  test("Number: integer renders without decimal") {
    val s = serialize(Number(5.0))
    assertContains(s, "<mn ")
    assertContains(s, ">5<")
  }

  test("Number: decimal keeps fractional part") {
    val s = serialize(Number(3.14))
    assertContains(s, "3.14")
  }

  test("Number: data-mathlify-id set on inner element") {
    val s = serialize(Number(1.0))
    assertContains(s, """data-mathlify-id="0"""")
  }

  // ── Symbol ────────────────────────────────────────────────────────────────────

  test("Symbol produces mi element") {
    val s = serialize(Symbol("x"))
    assertTagExists(s, "mi")
    assertContains(s, ">x<")
  }

  // ── Constant ─────────────────────────────────────────────────────────────────

  test("Constant pi renders as π") {
    val s = serialize(Constant("pi"))
    assertContains(s, ">π<")
  }

  test("Constant inf renders as ∞") {
    val s = serialize(Constant("inf"))
    assertContains(s, ">∞<")
  }

  // ── Add ───────────────────────────────────────────────────────────────────────

  test("Add produces mrow with + operator") {
    val s = serialize(Add(Symbol("x"), Number(1.0)))
    assertTagExists(s, "mrow")
    assertContains(s, ">+<")
  }

  // ── Sub ───────────────────────────────────────────────────────────────────────

  test("Sub produces mrow with - operator") {
    val s = serialize(Sub(Number(5.0), Number(3.0)))
    assertTagExists(s, "mrow")
    assertContains(s, ">-<")
  }

  // ── Mul ───────────────────────────────────────────────────────────────────────

  test("Mul produces mrow") {
    val s = serialize(Mul(Symbol("x"), Symbol("y")))
    assertTagExists(s, "mrow")
  }

  // ── Div ───────────────────────────────────────────────────────────────────────

  test("Div produces mfrac") {
    val s = serialize(Div(Symbol("a"), Symbol("b")))
    assertTagExists(s, "mfrac")
  }

  // ── Pow ───────────────────────────────────────────────────────────────────────

  test("Pow produces msup") {
    val s = serialize(Pow(Symbol("x"), Number(2.0)))
    assertTagExists(s, "msup")
  }

  // ── Neg ───────────────────────────────────────────────────────────────────────

  test("Neg produces mrow with leading minus") {
    val s = serialize(Neg(Symbol("x")))
    assertTagExists(s, "mrow")
    assertContains(s, ">-<")
  }

  // ── Fraction ─────────────────────────────────────────────────────────────────

  test("Fraction produces mfrac") {
    val s = serialize(Fraction(Number(1.0), Number(2.0)))
    assertTagExists(s, "mfrac")
  }

  // ── Root ─────────────────────────────────────────────────────────────────────

  test("Root(None) produces msqrt") {
    val s = serialize(Root(None, Symbol("x")))
    assertTagExists(s, "msqrt")
  }

  test("Root(Some) produces mroot") {
    val s = serialize(Root(Some(Number(3.0)), Symbol("x")))
    assertTagExists(s, "mroot")
  }

  // ── Sum ───────────────────────────────────────────────────────────────────────

  test("Sum produces mrow with munderover and ∑") {
    val s = serialize(Sum(Symbol("i"), Number(0.0), Symbol("n"), Symbol("x")))
    assertTagExists(s, "munderover")
    assertContains(s, ">∑<")
  }

  // ── Integral ─────────────────────────────────────────────────────────────────

  test("Integral produces mrow with munderover and ∫") {
    val s = serialize(Integral(Symbol("x"), Number(0.0), Number(1.0), Symbol("x")))
    assertTagExists(s, "munderover")
    assertContains(s, ">∫<")
  }

  // ── FunctionCall ─────────────────────────────────────────────────────────────

  test("FunctionCall produces mrow with name mi and parens") {
    val s = serialize(FunctionCall("sin", List(Symbol("x"))))
    assertTagExists(s, "mrow")
    assertContains(s, ">sin<")
    assertContains(s, ">(</")
    assertContains(s, ">)</")
  }

  // ── Subscript ────────────────────────────────────────────────────────────────

  test("Subscript produces msub") {
    val s = serialize(Subscript(Symbol("x"), Number(1.0)))
    assertTagExists(s, "msub")
  }

  // ── Superscript ──────────────────────────────────────────────────────────────

  test("Superscript produces msup") {
    val s = serialize(Superscript(Symbol("x"), Number(2.0)))
    assertTagExists(s, "msup")
  }

  // ── Matrix ───────────────────────────────────────────────────────────────────

  test("Matrix produces mtable with mtr and mtd") {
    val s = serialize(
      Matrix(List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d")), 2, 2, 2, 1, 0)
    )
    assertTagExists(s, "mtable")
    assertTagExists(s, "mtr")
    assertTagExists(s, "mtd")
  }

  // ── Style ────────────────────────────────────────────────────────────────────

  test("Style produces mstyle with mathvariant") {
    val s = serialize(Style("bold", Symbol("x")))
    assertTagExists(s, "mstyle")
    assertContains(s, """mathvariant="bold"""")
  }

  // ── Color ────────────────────────────────────────────────────────────────────

  test("Color produces mstyle with mathcolor") {
    val s = serialize(Color("red", Symbol("x")))
    assertTagExists(s, "mstyle")
    assertContains(s, """mathcolor="red"""")
  }

  // ── TextNode ─────────────────────────────────────────────────────────────────

  test("TextNode produces mtext") {
    val s = serialize(TextNode("hello"))
    assertTagExists(s, "mtext")
    assertContains(s, ">hello<")
  }

  // ── BracketGroup ─────────────────────────────────────────────────────────────

  test("BracketGroup with both brackets produces mrow with open/close mo") {
    val s = serialize(BracketGroup("(", ")", Symbol("x")))
    assertTagExists(s, "mrow")
    assertContains(s, ">(</")
    assertContains(s, ">)</")
  }

  test("BracketGroup with empty close omits close mo") {
    val s = serialize(BracketGroup("(", "", Symbol("x")))
    assertContains(s, ">(</")
    // no closing paren
    assert(!s.contains(">)<"), s"Unexpected close mo in: $s")
  }

  test("BracketGroup with empty open omits open mo") {
    val s = serialize(BracketGroup("", ")", Symbol("x")))
    assertContains(s, ">)</")
    assert(!s.contains(">(</"), s"Unexpected open mo in: $s")
  }

  // ── Enclose ──────────────────────────────────────────────────────────────────

  test("Enclose produces menclose with notation") {
    val s = serialize(Enclose("updiagonalstrike", Symbol("x")))
    assertTagExists(s, "menclose")
    assertContains(s, """notation="updiagonalstrike"""")
  }

  // ── Operator ─────────────────────────────────────────────────────────────────

  test("Operator produces mo element") {
    val s = serialize(Operator("∀"))
    assertTagExists(s, "mo")
    assertContains(s, ">∀<")
  }

  // ── ExprSeq ──────────────────────────────────────────────────────────────────

  test("ExprSeq produces mrow") {
    val s = serialize(ExprSeq(List(Symbol("x"), Operator("+"))))
    assertTagExists(s, "mrow")
  }

  // ── Over / Under / SubSup ────────────────────────────────────────────────────

  test("Over produces mover") {
    val s = serialize(Over(Symbol("x"), Operator("^")))
    assertTagExists(s, "mover")
  }

  test("Under produces munder") {
    val s = serialize(Under(Symbol("x"), Operator("_")))
    assertTagExists(s, "munder")
  }

  test("SubSup produces msubsup") {
    val s = serialize(SubSup(Symbol("x"), Number(1.0), Number(2.0)))
    assertTagExists(s, "msubsup")
  }

  // ── XML safety ───────────────────────────────────────────────────────────────

  test("escapes < in text content") {
    val s = serialize(TextNode("a<b"))
    assertContains(s, "&lt;")
  }

  test("escapes & in text content") {
    val s = serialize(TextNode("a&b"))
    assertContains(s, "&amp;")
  }

  // ── Parser round-trips ────────────────────────────────────────────────────────

  test("round-trip: x^2 + 2x + 1") {
    val s = MathParser.parse("x^2 + 2x + 1").map(MathMLSerializer.toMathML)
    assert(s.isRight, s"Parse failed: $s")
    val xml = s.toOption.get
    assertContains(xml, "<math ")
    assert(xml.length > 10, "MathML should be non-trivially long")
  }

  test("round-trip: int_0^1 x dx (via AsciiMath parser)") {
    val s = MathParser.parse("int_0^1 x").map(MathMLSerializer.toMathML)
    assert(s.isRight, s"Parse failed: $s")
    val xml = s.toOption.get
    assertContains(xml, "<math ")
  }

  test("round-trip: sum_{i=0}^{n} i") {
    val s = MathParser.parse("sum_{i=0}^{n} i").map(MathMLSerializer.toMathML)
    assert(s.isRight, s"Parse failed: $s")
    val xml = s.toOption.get
    assertContains(xml, "<math ")
  }

  test("round-trip: MathML is non-empty for simple expression") {
    val exprs = List("x", "42", "x+1", "sin(x)", "sqrt(x)")
    for input <- exprs do
      val result = MathParser.parse(input).map(MathMLSerializer.toMathML)
      assert(result.isRight, s"Parse failed for '$input': $result")
      val xml = result.toOption.get
      assert(xml.nonEmpty && xml.contains("<math"), s"Empty or invalid MathML for '$input'")
    end for
  }

end MathMLSerializerSuite
