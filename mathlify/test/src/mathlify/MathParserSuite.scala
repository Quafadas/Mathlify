package mathlify

class MathParserSuite extends munit.FunSuite:

  // ── atoms ────────────────────────────────────────────────────────────────

  test("parses a single variable"):
    assertEquals(MathParser.parse("x"), MathExpr.Var("x"))

  test("parses a multi-character identifier"):
    assertEquals(MathParser.parse("abc"), MathExpr.Var("abc"))

  test("parses an integer literal"):
    assertEquals(MathParser.parse("42"), MathExpr.Num("42"))

  test("parses a decimal literal"):
    assertEquals(MathParser.parse("3.14"), MathExpr.Num("3.14"))

  // ── arithmetic ───────────────────────────────────────────────────────────

  test("parses addition"):
    assertEquals(
      MathParser.parse("a + b"),
      MathExpr.Add(MathExpr.Var("a"), MathExpr.Var("b"))
    )

  test("parses subtraction"):
    assertEquals(
      MathParser.parse("a - b"),
      MathExpr.Sub(MathExpr.Var("a"), MathExpr.Var("b"))
    )

  test("parses multiplication"):
    assertEquals(
      MathParser.parse("a * b"),
      MathExpr.Mul(MathExpr.Var("a"), MathExpr.Var("b"))
    )

  test("parses division"):
    assertEquals(
      MathParser.parse("a / b"),
      MathExpr.Div(MathExpr.Var("a"), MathExpr.Var("b"))
    )

  // ── precedence ───────────────────────────────────────────────────────────

  test("multiplication binds tighter than addition"):
    assertEquals(
      MathParser.parse("a + b * c"),
      MathExpr.Add(
        MathExpr.Var("a"),
        MathExpr.Mul(MathExpr.Var("b"), MathExpr.Var("c"))
      )
    )

  test("addition is left-associative"):
    assertEquals(
      MathParser.parse("a + b + c"),
      MathExpr.Add(
        MathExpr.Add(MathExpr.Var("a"), MathExpr.Var("b")),
        MathExpr.Var("c")
      )
    )

  // ── parentheses ──────────────────────────────────────────────────────────

  test("respects parentheses"):
    assertEquals(
      MathParser.parse("(a + b) * c"),
      MathExpr.Mul(
        MathExpr.Parens(MathExpr.Add(MathExpr.Var("a"), MathExpr.Var("b"))),
        MathExpr.Var("c")
      )
    )

  // ── equations ────────────────────────────────────────────────────────────

  test("parses a simple equation"):
    assertEquals(
      MathParser.parse("a = b"),
      MathExpr.Eq(MathExpr.Var("a"), MathExpr.Var("b"))
    )

  test("parses the associativity law — the flagship example"):
    assertEquals(
      MathParser.parse("(a + b) + c = a + (b + c)"),
      MathExpr.Eq(
        MathExpr.Add(
          MathExpr.Parens(MathExpr.Add(MathExpr.Var("a"), MathExpr.Var("b"))),
          MathExpr.Var("c")
        ),
        MathExpr.Add(
          MathExpr.Var("a"),
          MathExpr.Parens(MathExpr.Add(MathExpr.Var("b"), MathExpr.Var("c")))
        )
      )
    )

  // ── string interpolator ──────────────────────────────────────────────────

  test("mathlify interpolator produces the same result as MathParser.parse"):
    import mathlify.*
    val via_interp = mathlify"(a + b) + c = a + (b + c)"
    val via_parse  = MathParser.parse("(a + b) + c = a + (b + c)")
    assertEquals(via_interp, via_parse)

  test("mathlify interpolator splices runtime values"):
    import mathlify.*
    val name = "x"
    assertEquals(mathlify"$name + 1", MathExpr.Add(MathExpr.Var("x"), MathExpr.Num("1")))

  // ── errors ───────────────────────────────────────────────────────────────

  test("throws MathParseException for an invalid character"):
    interceptMessage[MathParseException]("Unexpected character '@' at position 0"):
      MathParser.parse("@foo")

  test("throws MathParseException for unmatched parenthesis"):
    intercept[MathParseException]:
      MathParser.parse("(a + b")
