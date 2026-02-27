package mathlify

import MathExpr.*
import org.scalajs.dom
import org.scalatest.funsuite.AnyFunSuite

class AsciiMathSpec extends AnyFunSuite:

  // ── Helpers ───────────────────────────────────────────────────────────────

  private def check(input: String, expected: MathExpr): Unit =
    assert(AsciiMath.translate(input) == Right(expected), s"input: $input")

  // ── 1. Atoms ──────────────────────────────────────────────────────────────

  test("AM: empty string") {
    check("", Symbol(""))
  }

  test("AM: single letter") {
    check("x", Symbol("x"))
  }

  test("AM: integer number") {
    check("42", Number(42.0))
  }

  test("AM: decimal number") {
    check("3.14", Number(3.14))
  }

  // ── 2. Greek letters ──────────────────────────────────────────────────────

  test("AM: alpha") {
    check("alpha", Symbol("α"))
  }

  test("AM: pi (mi tag -> Symbol)") {
    check("pi", Symbol("π"))
  }

  test("AM: Delta (mo tag -> Operator)") {
    check("Delta", Operator("Δ"))
  }

  test("AM: theta") {
    check("theta", Symbol("θ"))
  }

  // ── 3. Logical / set symbols ──────────────────────────────────────────────

  test("AM: AA -> forall") {
    check("AA", Operator("∀"))
  }

  test("AM: EE -> exists") {
    check("EE", Operator("∃"))
  }

  test("AM: NN -> naturals") {
    check("NN", Operator("ℕ"))
  }

  test("AM: RR -> reals") {
    check("RR", Operator("ℝ"))
  }

  test("AM: in -> element-of") {
    check("in", Operator("∈"))
  }

  test("AM: sub -> subset") {
    check("sub", Operator("⊂"))
  }

  // ── 4. Operators ─────────────────────────────────────────────────────────

  test("AM: int") {
    check("int", Operator("∫"))
  }

  test("AM: sum") {
    check("sum", Operator("∑"))
  }

  test("AM: oo -> infinity") {
    check("oo", Operator("∞"))
  }

  test("AM: xx -> times") {
    check("xx", Operator("×"))
  }

  test("AM: !=") {
    check("!=", Operator("≠"))
  }

  test("AM: <= ") {
    check("<=", Operator("≤"))
  }

  test("AM: =>") {
    check("=>", Operator("⇒"))
  }

  // ── 5. Superscript / subscript ────────────────────────────────────────────

  test("AM: x^2") {
    check("x^2", Superscript(Symbol("x"), Number(2.0)))
  }

  test("AM: x_1") {
    check("x_1", Subscript(Symbol("x"), Number(1.0)))
  }

  test("AM: x_1^2 -> SubSup") {
    check("x_1^2", SubSup(Symbol("x"), Number(1.0), Number(2.0)))
  }

  test("AM: x^2_1 -> SubSup (reversed)") {
    check("x^2_1", SubSup(Symbol("x"), Number(1.0), Number(2.0)))
  }

  test("AM: a^(n+1)") {
    check(
      "a^(n+1)",
      Superscript(
        Symbol("a"),
        ExprSeq(List(Symbol("n"), Operator("+"), Number(1.0)))
      )
    )
  }

  // ── 6. Fractions ─────────────────────────────────────────────────────────

  test("AM: 2/3 -> Fraction") {
    check("2/3", Fraction(Number(2.0), Number(3.0)))
  }

  test("AM: frac(a)(b)") {
    check("frac(a)(b)", Fraction(Symbol("a"), Symbol("b")))
  }

  test("AM: (a+b)/(c+d)") {
    check(
      "(a+b)/(c+d)",
      Fraction(
        ExprSeq(List(Symbol("a"), Operator("+"), Symbol("b"))),
        ExprSeq(List(Symbol("c"), Operator("+"), Symbol("d")))
      )
    )
  }

  // ── 7. Roots ──────────────────────────────────────────────────────────────

  test("AM: sqrt(x)") {
    check("sqrt(x)", Root(None, Symbol("x")))
  }

  test("AM: sqrt x") {
    check("sqrt x", Root(None, Symbol("x")))
  }

  test("AM: root(3)(x)") {
    check("root(3)(x)", Root(Some(Number(3.0)), Symbol("x")))
  }

  // ── 8. Brackets ───────────────────────────────────────────────────────────

  test("AM: (x) -> BracketGroup") {
    check("(x)", BracketGroup("(", ")", Symbol("x")))
  }

  test("AM: [x] -> BracketGroup") {
    check("[x]", BracketGroup("[", "]", Symbol("x")))
  }

  test("AM: {x} -> BracketGroup") {
    check("{x}", BracketGroup("{", "}", Symbol("x")))
  }

  test("AM: |x| -> abs BracketGroup") {
    check("|x|", BracketGroup("∣", "∣", Symbol("x")))
  }

  test("AM: (: x :) -> angle BracketGroup") {
    check("(: x :)", BracketGroup("〈", "〉", Symbol("x")))
  }

  test("AM: << x >> -> angle BracketGroup") {
    check("<< x >>", BracketGroup("〈", "〉", Symbol("x")))
  }

  // ── 9. Abs / norm / floor / ceil ─────────────────────────────────────────

  test("AM: abs(x)") {
    check("abs(x)", BracketGroup("|", "|", Symbol("x")))
  }

  test("AM: norm(x)") {
    check("norm(x)", BracketGroup("∥", "∥", Symbol("x")))
  }

  test("AM: floor(x)") {
    check("floor(x)", BracketGroup("⌊", "⌋", Symbol("x")))
  }

  test("AM: ceil(x)") {
    check("ceil(x)", BracketGroup("⌈", "⌉", Symbol("x")))
  }

  // ── 10. Accents ───────────────────────────────────────────────────────────

  test("AM: hat x") {
    check("hat x", Over(Symbol("x"), Operator("^")))
  }

  test("AM: bar x") {
    check("bar x", Over(Symbol("x"), Operator("¯")))
  }

  test("AM: vec x") {
    check("vec x", Over(Symbol("x"), Operator("→")))
  }

  test("AM: dot x") {
    check("dot x", Over(Symbol("x"), Operator(".")))
  }

  test("AM: tilde x") {
    check("tilde x", Over(Symbol("x"), Operator("~")))
  }

  test("AM: ul x -> underline") {
    check("ul x", Under(Symbol("x"), Operator("\u0332")))
  }

  test("AM: hat(x+y)") {
    check(
      "hat(x+y)",
      Over(ExprSeq(List(Symbol("x"), Operator("+"), Symbol("y"))), Operator("^"))
    )
  }

  // ── 11. Stackrel / overset / underset ────────────────────────────────────

  test("AM: stackrel(A)(=)") {
    check("stackrel(A)(=)", Over(Operator("="), Symbol("A")))
  }

  test("AM: overset(A)(=)") {
    check("overset(A)(=)", Over(Operator("="), Symbol("A")))
  }

  test("AM: underset(A)(=)") {
    check("underset(A)(=)", Under(Operator("="), Symbol("A")))
  }

  // ── 12. Text and style ────────────────────────────────────────────────────

  test("AM: text(hello)") {
    check("text(hello)", TextNode("hello"))
  }

  test("AM: text{world}") {
    check("text{world}", TextNode("world"))
  }

  test("AM: bb x -> bold") {
    check("bb x", Style("bold", Symbol("x")))
  }

  test("AM: sf x -> sans-serif") {
    check("sf x", Style("sans-serif", Symbol("x")))
  }

  test("AM: bbb x -> double-struck") {
    check("bbb x", Style("double-struck", Symbol("x")))
  }

  test("AM: cc x -> script") {
    check("cc x", Style("script", Symbol("x")))
  }

  test("AM: tt x -> monospace") {
    check("tt x", Style("monospace", Symbol("x")))
  }

  test("AM: fr x -> fraktur") {
    check("fr x", Style("fraktur", Symbol("x")))
  }

  // ── 13. Cancel ────────────────────────────────────────────────────────────

  test("AM: cancel x") {
    check("cancel x", Enclose("updiagonalstrike", Symbol("x")))
  }

  test("AM: cancel(x+y)") {
    check(
      "cancel(x+y)",
      Enclose("updiagonalstrike", ExprSeq(List(Symbol("x"), Operator("+"), Symbol("y"))))
    )
  }

  // ── 14. Color ─────────────────────────────────────────────────────────────

  test("AM: color(red)(x)") {
    check("color(red)(x)", Color("red", Symbol("x")))
  }

  // ── 15. Sequences ─────────────────────────────────────────────────────────

  test("AM: x + y -> ExprSeq") {
    check(
      "x + y",
      ExprSeq(List(Symbol("x"), Operator("+"), Symbol("y")))
    )
  }

  test("AM: sin x -> func + arg sequence") {
    check("sin x", ExprSeq(List(Operator("sin"), Symbol("x"))))
  }

  test("AM: sin(x) -> func + bracket group") {
    check("sin(x)", ExprSeq(List(Operator("sin"), BracketGroup("(", ")", Symbol("x")))))
  }

  test("AM: lim_(x->0) -> Under") {
    check(
      "lim_(x->0)",
      Under(
        Operator("lim"),
        ExprSeq(List(Symbol("x"), Operator("→"), Number(0.0)))
      )
    )
  }

  test("AM: sum_(i=1)^n -> SubSup") {
    check(
      "sum_(i=1)^n",
      SubSup(
        Operator("∑"),
        ExprSeq(List(Symbol("i"), Operator("="), Number(1.0))),
        Symbol("n")
      )
    )
  }

  // ── 16. Definition expansion ─────────────────────────────────────────────

  test("AM: dx expands to d x sequence") {
    check("dx", ExprSeq(List(Symbol("d"), Symbol("x"))))
  }

  test("AM: divide expands to -:") {
    check("a divide b", ExprSeq(List(Symbol("a"), Operator("÷"), Symbol("b"))))
  }

  // ── 17. MathML DOM compilation of new nodes ──────────────────────────────

  test("MathML: Operator produces mo") {
    val e = MathMLCompiler.compile(Operator("∀"), "0")
    assert(e.tagName.toLowerCase == "mo")
    assert(e.textContent == "∀")
  }

  test("MathML: ExprSeq produces mrow") {
    val e = MathMLCompiler.compile(ExprSeq(List(Symbol("x"), Operator("+"))), "0")
    assert(e.tagName.toLowerCase == "mrow")
    assert(e.childNodes.length == 2)
  }

  test("MathML: Over produces mover") {
    val e = MathMLCompiler.compile(Over(Symbol("x"), Operator("^")), "0")
    assert(e.tagName.toLowerCase == "mover")
    assert(e.childNodes.length == 2)
  }

  test("MathML: Under produces munder") {
    val e = MathMLCompiler.compile(Under(Symbol("x"), Operator("_")), "0")
    assert(e.tagName.toLowerCase == "munder")
    assert(e.childNodes.length == 2)
  }

  test("MathML: SubSup produces msubsup") {
    val e = MathMLCompiler.compile(SubSup(Symbol("x"), Number(1.0), Number(2.0)), "0")
    assert(e.tagName.toLowerCase == "msubsup")
    assert(e.childNodes.length == 3)
  }

  test("MathML: Style produces mstyle with mathvariant") {
    val e = MathMLCompiler.compile(Style("bold", Symbol("x")), "0")
    assert(e.tagName.toLowerCase == "mstyle")
    assert(e.getAttribute("mathvariant") == "bold")
  }

  test("MathML: TextNode produces mtext") {
    val e = MathMLCompiler.compile(TextNode("hello"), "0")
    assert(e.tagName.toLowerCase == "mtext")
    assert(e.textContent == "hello")
  }

  test("MathML: BracketGroup produces mrow with open and close mo") {
    val e = MathMLCompiler.compile(BracketGroup("(", ")", Symbol("x")), "0")
    assert(e.tagName.toLowerCase == "mrow")
    assert(e.childNodes.length == 3)
    assert(e.firstChild.asInstanceOf[dom.Element].textContent == "(")
    assert(e.lastChild.asInstanceOf[dom.Element].textContent == ")")
  }

  test("MathML: BracketGroup with empty close omits close mo") {
    val e = MathMLCompiler.compile(BracketGroup("(", "", Symbol("x")), "0")
    assert(e.tagName.toLowerCase == "mrow")
    assert(e.childNodes.length == 2)
  }

  test("MathML: Enclose produces menclose") {
    val e = MathMLCompiler.compile(Enclose("updiagonalstrike", Symbol("x")), "0")
    assert(e.tagName.toLowerCase == "menclose")
    assert(e.getAttribute("notation") == "updiagonalstrike")
  }

  test("MathML: Color produces mstyle with mathcolor") {
    val e = MathMLCompiler.compile(Color("red", Symbol("x")), "0")
    assert(e.tagName.toLowerCase == "mstyle")
    assert(e.getAttribute("mathcolor") == "red")
  }

  test("MathML: toMathML round-trip for Superscript via AsciiMath") {
    AsciiMath.translate("x^2") match
      case Right(ast) =>
        val math = MathMLCompiler.toMathML(ast)
        assert(math.tagName.toLowerCase == "math")
        val msup = math.firstChild.asInstanceOf[dom.Element]
        assert(msup.tagName.toLowerCase == "msup")
      case Left(err) => fail(s"parse error: $err")
  }

  test("MathML: toMathML round-trip for Fraction via AsciiMath") {
    AsciiMath.translate("2/3") match
      case Right(ast) =>
        val math = MathMLCompiler.toMathML(ast)
        val mfrac = math.firstChild.asInstanceOf[dom.Element]
        assert(mfrac.tagName.toLowerCase == "mfrac")
      case Left(err) => fail(s"parse error: $err")
  }

  // ── 18. Matrix ────────────────────────────────────────────────────────────

  test("AM: [[a,b],[c,d]] -> matrix") {
    check(
      "[[a,b],[c,d]]",
      BracketGroup(
        "[",
        "]",
        Matrix(
          List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d")),
          2,
          2,
          2,
          1,
          0
        )
      )
    )
  }

  test("AM: [[1,2],[3,4],[5,6]] -> 3x2 matrix") {
    check(
      "[[1,2],[3,4],[5,6]]",
      BracketGroup(
        "[",
        "]",
        Matrix(
          List(Number(1.0), Number(2.0), Number(3.0), Number(4.0), Number(5.0), Number(6.0)),
          3,
          2,
          2,
          1,
          0
        )
      )
    )
  }

  test("AM: [[a,b]] (single row) -> plain bracket group, not matrix") {
    check(
      "[[a,b]]",
      BracketGroup(
        "[",
        "]",
        BracketGroup("[", "]", ExprSeq(List(Symbol("a"), Operator(","), Symbol("b"))))
      )
    )
  }

  test("AM: [[a,b],[c]] (inconsistent columns) -> plain bracket group, not matrix") {
    check(
      "[[a,b],[c]]",
      BracketGroup(
        "[",
        "]",
        ExprSeq(
          List(
            BracketGroup("[", "]", ExprSeq(List(Symbol("a"), Operator(","), Symbol("b")))),
            Operator(","),
            BracketGroup("[", "]", Symbol("c"))
          )
        )
      )
    )
  }

  test("MathML: [[a,b],[c,d]] produces mrow with mtable inside brackets") {
    AsciiMath.translate("[[a,b],[c,d]]") match
      case Right(ast) =>
        val math = MathMLCompiler.toMathML(ast)
        val mrow = math.firstChild.asInstanceOf[dom.Element]
        assert(mrow.tagName.toLowerCase == "mrow")
        assert(mrow.childNodes.length == 3)
        assert(mrow.firstChild.asInstanceOf[dom.Element].textContent == "[")
        val mtable = mrow.childNodes.item(1).asInstanceOf[dom.Element]
        assert(mtable.tagName.toLowerCase == "mtable")
        assert(mtable.childNodes.length == 2)
        assert(mrow.lastChild.asInstanceOf[dom.Element].textContent == "]")
      case Left(err) => fail(s"parse error: $err")
  }
