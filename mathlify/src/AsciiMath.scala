package mathlify

import MathExpr.*

object AsciiMath:

  // ── Token types ──────────────────────────────────────────────────────────

  sealed trait TType
  case object CONST         extends TType
  case object UNARY         extends TType
  case object BINARY        extends TType
  case object INFIX         extends TType
  case object LEFTBRACKET   extends TType
  case object RIGHTBRACKET  extends TType
  case object SPACE         extends TType
  case object UNDEROVER     extends TType
  case object DEFINITION    extends TType
  case object LEFTRIGHT     extends TType
  case object TEXTTYPE      extends TType
  case object UNARYUNDEROVER extends TType

  case class AMSym(
    input: String,
    output: String,
    tag: String,
    ttype: TType,
    func: Boolean = false,
    acc: Boolean = false,
    rewriteleftright: Option[(String, String)] = None,
    invisible: Boolean = false
  )

  // ── Symbol table ─────────────────────────────────────────────────────────

  val rawSymbols: List[AMSym] = List(
    // Greek letters
    AMSym("alpha",      "α",  "mi", CONST),
    AMSym("beta",       "β",  "mi", CONST),
    AMSym("chi",        "χ",  "mi", CONST),
    AMSym("delta",      "δ",  "mi", CONST),
    AMSym("Delta",      "Δ",  "mo", CONST),
    AMSym("epsi",       "ε",  "mi", CONST),
    AMSym("epsilon",    "ε",  "mi", CONST),
    AMSym("varepsilon", "ɛ",  "mi", CONST),
    AMSym("eta",        "η",  "mi", CONST),
    AMSym("gamma",      "γ",  "mi", CONST),
    AMSym("Gamma",      "Γ",  "mo", CONST),
    AMSym("iota",       "ι",  "mi", CONST),
    AMSym("kappa",      "κ",  "mi", CONST),
    AMSym("lambda",     "λ",  "mi", CONST),
    AMSym("Lambda",     "Λ",  "mo", CONST),
    AMSym("lamda",      "λ",  "mi", CONST),
    AMSym("Lamda",      "Λ",  "mo", CONST),
    AMSym("mu",         "μ",  "mi", CONST),
    AMSym("nu",         "ν",  "mi", CONST),
    AMSym("omega",      "ω",  "mi", CONST),
    AMSym("Omega",      "Ω",  "mo", CONST),
    AMSym("phi",        "ϕ",  "mi", CONST),
    AMSym("varphi",     "φ",  "mi", CONST),
    AMSym("Phi",        "Φ",  "mo", CONST),
    AMSym("pi",         "π",  "mi", CONST),
    AMSym("Pi",         "Π",  "mo", CONST),
    AMSym("psi",        "ψ",  "mi", CONST),
    AMSym("Psi",        "Ψ",  "mi", CONST),
    AMSym("rho",        "ρ",  "mi", CONST),
    AMSym("sigma",      "σ",  "mi", CONST),
    AMSym("Sigma",      "Σ",  "mo", CONST),
    AMSym("tau",        "τ",  "mi", CONST),
    AMSym("theta",      "θ",  "mi", CONST),
    AMSym("vartheta",   "ϑ",  "mi", CONST),
    AMSym("Theta",      "Θ",  "mo", CONST),
    AMSym("upsilon",    "υ",  "mi", CONST),
    AMSym("xi",         "ξ",  "mi", CONST),
    AMSym("Xi",         "Ξ",  "mo", CONST),
    AMSym("zeta",       "ζ",  "mi", CONST),
    // Binary operations
    AMSym("***",        "⋆",  "mo", CONST),
    AMSym("**",         "∗",  "mo", CONST),
    AMSym("*",          "⋅",  "mo", CONST),
    AMSym("//",         "/",  "mo", CONST),
    AMSym("\\\\",       "\\", "mo", CONST),
    AMSym("setminus",   "\\", "mo", CONST),
    AMSym("xx",         "×",  "mo", CONST),
    AMSym("|><|",       "⋈",  "mo", CONST),
    AMSym("|><",        "⋉",  "mo", CONST),
    AMSym("><|",        "⋊",  "mo", CONST),
    AMSym("-:",         "÷",  "mo", CONST),
    AMSym("divide",     "-:", "mo", DEFINITION),
    AMSym("@",          "∘",  "mo", CONST),
    AMSym("oplus",      "⊕",  "mo", CONST),
    AMSym("o+",         "⊕",  "mo", CONST),
    AMSym("ominus",     "⊖",  "mo", CONST),
    AMSym("o-",         "⊖",  "mo", CONST),
    AMSym("otimes",     "⊗",  "mo", CONST),
    AMSym("ox",         "⊗",  "mo", CONST),
    AMSym("odot",       "⊙",  "mo", CONST),
    AMSym("o.",         "⊙",  "mo", CONST),
    AMSym("sum",        "∑",  "mo", UNDEROVER),
    AMSym("prod",       "∏",  "mo", UNDEROVER),
    AMSym("^^^",        "⋀",  "mo", UNDEROVER),
    AMSym("^^",         "∧",  "mo", CONST),
    AMSym("vvv",        "⋁",  "mo", UNDEROVER),
    AMSym("vv",         "∨",  "mo", CONST),
    AMSym("nnn",        "⋂",  "mo", UNDEROVER),
    AMSym("nn",         "∩",  "mo", CONST),
    AMSym("uuu",        "⋃",  "mo", UNDEROVER),
    AMSym("uu",         "∪",  "mo", CONST),
    AMSym("dagger",     "†",  "mo", CONST),
    AMSym("dag",        "†",  "mo", CONST),
    AMSym("ddagger",    "‡",  "mo", CONST),
    AMSym("ddag",       "‡",  "mo", CONST),
    // Binary relations
    AMSym("!=",         "≠",  "mo", CONST),
    AMSym("ne",         "≠",  "mo", CONST),
    AMSym(":=",         ":=", "mo", CONST),
    AMSym("lt",         "<",  "mo", CONST),
    AMSym("leq",        "≤",  "mo", CONST),
    AMSym("le",         "≤",  "mo", CONST),
    AMSym("lt=",        "≤",  "mo", CONST),
    AMSym("<=",         "≤",  "mo", CONST),
    AMSym("gt",         ">",  "mo", CONST),
    AMSym("geq",        "≥",  "mo", CONST),
    AMSym("ge",         "≥",  "mo", CONST),
    AMSym("gt=",        "≥",  "mo", CONST),
    AMSym(">=",         "≥",  "mo", CONST),
    AMSym("mlt",        "≪",  "mo", CONST),
    AMSym("mgt",        "≫",  "mo", CONST),
    AMSym("-lt",        "≺",  "mo", CONST),
    AMSym("-<=",        "⪯",  "mo", CONST),
    AMSym("preceq",     "⪯",  "mo", CONST),
    AMSym("-<",         "≺",  "mo", CONST),
    AMSym("prec",       "≺",  "mo", CONST),
    AMSym(">-=",        "⪰",  "mo", CONST),
    AMSym("succeq",     "⪰",  "mo", CONST),
    AMSym(">-",         "≻",  "mo", CONST),
    AMSym("succ",       "≻",  "mo", CONST),
    AMSym("!in",        "∉",  "mo", CONST),
    AMSym("notin",      "∉",  "mo", CONST),
    AMSym("in",         "∈",  "mo", CONST),
    AMSym("!sub",       "⊄",  "mo", CONST),
    AMSym("notsubset",  "!sub","mo", DEFINITION),
    AMSym("subset",     "⊂",  "mo", CONST),
    AMSym("sub",        "⊂",  "mo", CONST),
    AMSym("!sup",       "⊅",  "mo", CONST),
    AMSym("notsupset",  "!sup","mo", DEFINITION),
    AMSym("supset",     "⊃",  "mo", CONST),
    AMSym("sup",        "⊃",  "mo", CONST),
    AMSym("!sube",      "⊈",  "mo", CONST),
    AMSym("notsubseteq","!sube","mo", DEFINITION),
    AMSym("subseteq",   "⊆",  "mo", CONST),
    AMSym("sube",       "⊆",  "mo", CONST),
    AMSym("!supe",      "⊉",  "mo", CONST),
    AMSym("notsupseteq","!supe","mo", DEFINITION),
    AMSym("supseteq",   "⊇",  "mo", CONST),
    AMSym("supe",       "⊇",  "mo", CONST),
    AMSym("!-=",        "≢",  "mo", CONST),
    AMSym("notequiv",   "!-=","mo", DEFINITION),
    AMSym("-=",         "≡",  "mo", CONST),
    AMSym("equiv",      "≡",  "mo", CONST),
    AMSym("~=",         "≅",  "mo", CONST),
    AMSym("cong",       "≅",  "mo", CONST),
    AMSym("~~",         "≈",  "mo", CONST),
    AMSym("approx",     "≈",  "mo", CONST),
    AMSym("~",          "∼",  "mo", CONST),
    AMSym("sim",        "∼",  "mo", CONST),
    AMSym("propto",     "∝",  "mo", CONST),
    AMSym("prop",       "∝",  "mo", CONST),
    // Logical
    AMSym("and",        "and","mtext", SPACE),
    AMSym("or",         "or", "mtext", SPACE),
    AMSym("not",        "¬",  "mo", CONST),
    AMSym("neg",        "¬",  "mo", CONST),
    AMSym("implies",    "⇒",  "mo", CONST),
    AMSym("=>",         "⇒",  "mo", CONST),
    AMSym("if",         "if", "mo",  SPACE),
    AMSym("iff",        "⇔",  "mo", CONST),
    AMSym("<=>",        "⇔",  "mo", CONST),
    AMSym("forall",     "∀",  "mo", CONST),
    AMSym("AA",         "∀",  "mo", CONST),
    AMSym("exists",     "∃",  "mo", CONST),
    AMSym("EE",         "∃",  "mo", CONST),
    AMSym("_|_",        "⊥",  "mo", CONST),
    AMSym("bot",        "⊥",  "mo", CONST),
    AMSym("TT",         "⊤",  "mo", CONST),
    AMSym("top",        "⊤",  "mo", CONST),
    AMSym("|--",        "⊢",  "mo", CONST),
    AMSym("vdash",      "⊢",  "mo", CONST),
    AMSym("|==",        "⊨",  "mo", CONST),
    AMSym("models",     "⊨",  "mo", CONST),
    // Grouping brackets
    AMSym("(:",         "〈",  "mo", LEFTBRACKET),
    AMSym(":)",         "〉",  "mo", RIGHTBRACKET),
    AMSym("<<",         "〈",  "mo", LEFTBRACKET),
    AMSym(">>",         "〉",  "mo", RIGHTBRACKET),
    AMSym("{:",         "{:", "mo", LEFTBRACKET,  invisible = true),
    AMSym(":}",         ":}", "mo", RIGHTBRACKET, invisible = true),
    AMSym("(",          "(",  "mo", LEFTBRACKET),
    AMSym(")",          ")",  "mo", RIGHTBRACKET),
    AMSym("[",          "[",  "mo", LEFTBRACKET),
    AMSym("]",          "]",  "mo", RIGHTBRACKET),
    AMSym("{",          "{",  "mo", LEFTBRACKET),
    AMSym("}",          "}",  "mo", RIGHTBRACKET),
    AMSym("|",          "∣",  "mo", LEFTRIGHT),
    // Miscellaneous
    AMSym("oint",       "∮",  "mo", CONST),
    AMSym("int",        "∫",  "mo", CONST),
    AMSym("partial",    "∂",  "mo", CONST),
    AMSym("del",        "∂",  "mo", CONST),
    AMSym("nabla",      "∇",  "mo", CONST),
    AMSym("grad",       "∇",  "mo", CONST),
    AMSym("pm",         "±",  "mo", CONST),
    AMSym("+-",         "±",  "mo", CONST),
    AMSym("mp",         "∓",  "mo", CONST),
    AMSym("-+",         "∓",  "mo", CONST),
    AMSym("emptyset",   "∅",  "mo", CONST),
    AMSym("O/",         "∅",  "mo", CONST),
    AMSym("infty",      "∞",  "mo", CONST),
    AMSym("oo",         "∞",  "mo", CONST),
    AMSym("aleph",      "ℵ",  "mo", CONST),
    AMSym("ldots",      "...", "mo", CONST),
    AMSym("...",        "...", "mo", CONST),
    AMSym("therefore",  "∴",  "mo", CONST),
    AMSym(":.",         "∴",  "mo", CONST),
    AMSym("because",    "∵",  "mo", CONST),
    AMSym(":'",         "∵",  "mo", CONST),
    AMSym("angle",      "∠",  "mo", CONST),
    AMSym("/_",         "∠",  "mo", CONST),
    AMSym("triangle",   "△",  "mo", CONST),
    AMSym("/_\\",       "△",  "mo", CONST),
    AMSym("prime",      "′",  "mo", CONST),
    AMSym("'",          "′",  "mo", CONST),
    AMSym("\\ ",        "\u00A0", "mo", CONST),
    AMSym("frown",      "⌢",  "mo", CONST),
    AMSym("qquad",      "\u00A0\u00A0\u00A0\u00A0", "mo", CONST),
    AMSym("quad",       "\u00A0\u00A0", "mo", CONST),
    AMSym("cdots",      "⋯",  "mo", CONST),
    AMSym("vdots",      "⋮",  "mo", CONST),
    AMSym("ddots",      "⋱",  "mo", CONST),
    AMSym("diamond",    "⋄",  "mo", CONST),
    AMSym("square",     "□",  "mo", CONST),
    AMSym("lfloor",     "⌊",  "mo", CONST),
    AMSym("|__",        "⌊",  "mo", CONST),
    AMSym("rfloor",     "⌋",  "mo", CONST),
    AMSym("__|",        "⌋",  "mo", CONST),
    AMSym("lceiling",   "⌈",  "mo", CONST),
    AMSym("|~",         "⌈",  "mo", CONST),
    AMSym("rceiling",   "⌉",  "mo", CONST),
    AMSym("~|",         "⌉",  "mo", CONST),
    AMSym("CC",         "ℂ",  "mo", CONST),
    AMSym("NN",         "ℕ",  "mo", CONST),
    AMSym("QQ",         "ℚ",  "mo", CONST),
    AMSym("RR",         "ℝ",  "mo", CONST),
    AMSym("ZZ",         "ℤ",  "mo", CONST),
    AMSym("hbar",       "ℏ",  "mo", CONST),
    AMSym("f",          "f",  "mi", UNARY, func = true),
    AMSym("g",          "g",  "mi", UNARY, func = true),
    // Standard functions
    AMSym("Lim",        "Lim","mo", UNDEROVER),
    AMSym("lim",        "lim","mo", UNDEROVER),
    AMSym("arcsin",     "arcsin","mo", UNARY, func = true),
    AMSym("arccos",     "arccos","mo", UNARY, func = true),
    AMSym("arctan",     "arctan","mo", UNARY, func = true),
    AMSym("arcsec",     "arcsec","mo", UNARY, func = true),
    AMSym("arccsc",     "arccsc","mo", UNARY, func = true),
    AMSym("arccot",     "arccot","mo", UNARY, func = true),
    AMSym("Arcsin",     "Arcsin","mo", UNARY, func = true),
    AMSym("Arccos",     "Arccos","mo", UNARY, func = true),
    AMSym("Arctan",     "Arctan","mo", UNARY, func = true),
    AMSym("sinh",       "sinh","mo", UNARY, func = true),
    AMSym("cosh",       "cosh","mo", UNARY, func = true),
    AMSym("tanh",       "tanh","mo", UNARY, func = true),
    AMSym("coth",       "coth","mo", UNARY, func = true),
    AMSym("sech",       "sech","mo", UNARY, func = true),
    AMSym("csch",       "csch","mo", UNARY, func = true),
    AMSym("Sinh",       "Sinh","mo", UNARY, func = true),
    AMSym("Cosh",       "Cosh","mo", UNARY, func = true),
    AMSym("Tanh",       "Tanh","mo", UNARY, func = true),
    AMSym("sin",        "sin","mo", UNARY, func = true),
    AMSym("cos",        "cos","mo", UNARY, func = true),
    AMSym("tan",        "tan","mo", UNARY, func = true),
    AMSym("cot",        "cot","mo", UNARY, func = true),
    AMSym("sec",        "sec","mo", UNARY, func = true),
    AMSym("csc",        "csc","mo", UNARY, func = true),
    AMSym("Sin",        "Sin","mo", UNARY, func = true),
    AMSym("Cos",        "Cos","mo", UNARY, func = true),
    AMSym("Tan",        "Tan","mo", UNARY, func = true),
    AMSym("Cot",        "Cot","mo", UNARY, func = true),
    AMSym("Sec",        "Sec","mo", UNARY, func = true),
    AMSym("Csc",        "Csc","mo", UNARY, func = true),
    AMSym("exp",        "exp","mo", UNARY, func = true),
    AMSym("log",        "log","mo", UNARY, func = true),
    AMSym("ln",         "ln", "mo", UNARY, func = true),
    AMSym("det",        "det","mo", UNARY, func = true),
    AMSym("dim",        "dim","mo", CONST),
    AMSym("mod",        "mod","mo", CONST),
    AMSym("gcd",        "gcd","mo", UNARY, func = true),
    AMSym("lcm",        "lcm","mo", UNARY, func = true),
    AMSym("lub",        "lub","mo", CONST),
    AMSym("glb",        "glb","mo", CONST),
    AMSym("max",        "max","mo", UNDEROVER),
    AMSym("min",        "min","mo", UNDEROVER),
    AMSym("Log",        "Log","mo", UNARY, func = true),
    AMSym("Ln",         "Ln", "mo", UNARY, func = true),
    AMSym("abs",        "abs","mo", UNARY, rewriteleftright = Some(("|","|"))),
    AMSym("Abs",        "abs","mo", UNARY, rewriteleftright = Some(("|","|"))),
    AMSym("norm",       "norm","mo",UNARY, rewriteleftright = Some(("∥","∥"))),
    AMSym("floor",      "floor","mo",UNARY,rewriteleftright = Some(("⌊","⌋"))),
    AMSym("ceil",       "ceil","mo", UNARY,rewriteleftright = Some(("⌈","⌉"))),
    // Arrows
    AMSym("uparrow",    "↑",  "mo", CONST),
    AMSym("uarr",       "↑",  "mo", CONST),
    AMSym("Downarrow",  "⇓",  "mo", CONST),
    AMSym("downarrow",  "↓",  "mo", CONST),
    AMSym("dArr",       "⇓",  "mo", CONST),
    AMSym("darr",       "↓",  "mo", CONST),
    AMSym("rightarrowtail", "↣", "mo", CONST),
    AMSym("rightarrow", "→",  "mo", CONST),
    AMSym("twoheadrightarrowtail", "⤖", "mo", CONST),
    AMSym("twoheadrightarrow",     "↠", "mo", CONST),
    AMSym(">->>",       "⤖",  "mo", CONST),
    AMSym(">->",        "↣",  "mo", CONST),
    AMSym("->>",        "↠",  "mo", CONST),
    AMSym("->",         "→",  "mo", CONST),
    AMSym("to",         "→",  "mo", CONST),
    AMSym("|->",        "↦",  "mo", CONST),
    AMSym("mapsto",     "↦",  "mo", CONST),
    AMSym("Leftarrow",  "⇐",  "mo", CONST),
    AMSym("leftarrow",  "←",  "mo", CONST),
    AMSym("lArr",       "⇐",  "mo", CONST),
    AMSym("larr",       "←",  "mo", CONST),
    AMSym("Leftrightarrow","⇔","mo", CONST),
    AMSym("leftrightarrow","↔","mo", CONST),
    AMSym("hArr",       "⇔",  "mo", CONST),
    AMSym("harr",       "↔",  "mo", CONST),
    AMSym("Rightarrow", "⇒",  "mo", CONST),
    AMSym("rArr",       "⇒",  "mo", CONST),
    AMSym("rarr",       "→",  "mo", CONST),
    AMSym("rightleftharpoons","⇌","mo",CONST),
    // Commands with arguments
    AMSym("sqrt",       "sqrt","msqrt", UNARY),
    AMSym("root",       "root","mroot", BINARY),
    AMSym("frac",       "/",  "mfrac", BINARY),
    AMSym("/",          "/",  "mfrac", INFIX),
    AMSym("stackrel",   "stackrel","mover", BINARY),
    AMSym("overset",    "stackrel","mover", BINARY),
    AMSym("underset",   "stackrel","munder",BINARY),
    AMSym("_",          "_",  "msub",  INFIX),
    AMSym("^",          "^",  "msup",  INFIX),
    AMSym("hat",        "^",  "mover", UNARY, acc = true),
    AMSym("bar",        "¯",  "mover", UNARY, acc = true),
    AMSym("overline",   "¯",  "mover", UNARY, acc = true),
    AMSym("vec",        "→",  "mover", UNARY, acc = true),
    AMSym("dot",        ".",  "mover", UNARY, acc = true),
    AMSym("ddot",       "..","mover",  UNARY, acc = true),
    AMSym("overparen",  "⏜", "mover",  UNARY, acc = true),
    AMSym("overarc",    "⏜", "mover",  UNARY, acc = true),
    AMSym("underline",  "\u0332","munder",UNARY, acc = true),
    AMSym("ul",         "\u0332","munder",UNARY, acc = true),
    AMSym("underbrace", "⏟", "munder", UNARYUNDEROVER, acc = true),
    AMSym("ubrace",     "⏟", "munder", UNARYUNDEROVER, acc = true),
    AMSym("overbrace",  "⏞", "mover",  UNARYUNDEROVER, acc = true),
    AMSym("obrace",     "⏞", "mover",  UNARYUNDEROVER, acc = true),
    AMSym("tilde",      "~",  "mover", UNARY, acc = true),
    AMSym("mbox",       "mbox","mtext", TEXTTYPE),
    AMSym("text",       "text","mtext", TEXTTYPE),
    AMSym("color",      "",   "mstyle",BINARY),
    AMSym("cancel",     "cancel","menclose",UNARY),
    AMSym("mathbf",     "bold","mstyle",UNARY),
    AMSym("bb",         "bold","mstyle",UNARY),
    AMSym("mathsf",     "sans-serif","mstyle",UNARY),
    AMSym("sf",         "sans-serif","mstyle",UNARY),
    AMSym("mathbb",     "double-struck","mstyle",UNARY),
    AMSym("bbb",        "double-struck","mstyle",UNARY),
    AMSym("mathcal",    "script","mstyle",UNARY),
    AMSym("cc",         "script","mstyle",UNARY),
    AMSym("mathtt",     "monospace","mstyle",UNARY),
    AMSym("tt",         "monospace","mstyle",UNARY),
    AMSym("mathfrak",   "fraktur","mstyle",UNARY),
    AMSym("fr",         "fraktur","mstyle",UNARY),
    // dx/dy/dz/dt definitions
    AMSym("dx",         "{:d x:}","mi", DEFINITION),
    AMSym("dy",         "{:d y:}","mi", DEFINITION),
    AMSym("dz",         "{:d z:}","mi", DEFINITION),
    AMSym("dt",         "{:d t:}","mi", DEFINITION),
  )

  // Sorted longest-first for greedy matching
  val symbolMap: Map[String, AMSym] =
    rawSymbols.map(s => s.input -> s).toMap
  private val sortedNames: Array[String] =
    rawSymbols.map(_.input).toArray.sortBy(-_.length)

  // ── Lexer helpers ─────────────────────────────────────────────────────────

  private def getSymbol(str: String): Option[AMSym] =
    sortedNames.find(str.startsWith).flatMap(symbolMap.get)

  private def skipWS(str: String): String =
    str.dropWhile(_ <= ' ')

  // ── Public entry point ───────────────────────────────────────────────────

  def translate(input: String): Either[String, MathExpr] =
    val (exprs, _) = parseExpr(input.trim, rightbracket = false, depth = 0)
    Right(exprs match
      case Nil      => Symbol("")
      case List(e)  => e
      case es       => ExprSeq(es))

  // ── Grammar: E -> (I (/I)?)*  ─────────────────────────────────────────────

  private def parseExpr(
    str: String,
    rightbracket: Boolean,
    depth: Int
  ): (List[MathExpr], String) =
    var current = str
    var result  = List.empty[MathExpr]
    var continue = true
    while continue do
      current = skipWS(current)
      val sym = getSymbol(current)
      if current.isEmpty
        || sym.exists(s => s.ttype == RIGHTBRACKET && depth > 0)
        || sym.exists(s => s.ttype == LEFTRIGHT && rightbracket)
      then
        continue = false
      else
        val (nodeOpt, rest0) = parseIexpr(current, depth)
        nodeOpt match
          case None =>
            continue = false
          case Some(node) =>
            val s1     = skipWS(rest0)
            val fracSym = getSymbol(s1)
            if fracSym.exists(s => s.ttype == INFIX && s.input == "/") then
              val rest1 = skipWS(s1.substring(1))
              val (arg2Opt, rest2) = parseIexpr(rest1, depth)
              val arg2 = arg2Opt.getOrElse(Operator("□"))
              result  = result :+ Fraction(stripBrackets(node), stripBrackets(arg2))
              current = rest2
            else
              result  = result :+ node
              current = rest0
    (result, current)

  // ── Grammar: I -> S (_ S (^ S)? | ^ S (_ S)?)?  ─────────────────────────

  private def parseIexpr(str: String, depth: Int): (Option[MathExpr], String) =
    val sym1Opt = getSymbol(skipWS(str))
    val (nodeOpt, rest0) = parseSexpr(str, depth)
    nodeOpt match
      case None => (None, rest0)
      case Some(node) =>
        val s1  = skipWS(rest0)
        val sym = getSymbol(s1)
        sym match
          case Some(infix) if infix.ttype == INFIX && infix.input != "/" =>
            val rest1 = skipWS(s1.substring(infix.input.length))
            val (argOpt, rest2) = parseSexpr(rest1, depth)
            val arg     = stripBrackets(argOpt.getOrElse(Operator("□")))
            val isUO    = sym1Opt.exists(s => s.ttype == UNDEROVER || s.ttype == UNARYUNDEROVER)
            if infix.input == "_" then
              // look ahead for optional ^
              val s2 = skipWS(rest2)
              getSymbol(s2) match
                case Some(sup) if sup.input == "^" =>
                  val rest3 = skipWS(s2.substring(1))
                  val (arg2Opt, rest4) = parseSexpr(rest3, depth)
                  val arg2 = stripBrackets(arg2Opt.getOrElse(Operator("□")))
                  val combined =
                    if isUO then SubSup(node, arg, arg2)
                    else SubSup(node, arg, arg2)
                  (Some(combined), rest4)
                case _ =>
                  val combined =
                    if isUO then Under(node, arg)
                    else Subscript(node, arg)
                  (Some(combined), rest2)
            else // "^"
              // look ahead for optional _
              val s2 = skipWS(rest2)
              getSymbol(s2) match
                case Some(sub) if sub.input == "_" =>
                  val rest3 = skipWS(s2.substring(1))
                  val (arg2Opt, rest4) = parseSexpr(rest3, depth)
                  val arg2 = stripBrackets(arg2Opt.getOrElse(Operator("□")))
                  val combined =
                    if isUO then SubSup(node, arg2, arg)
                    else SubSup(node, arg2, arg)
                  (Some(combined), rest4)
                case _ =>
                  val combined =
                    if isUO then Over(node, arg)
                    else Superscript(node, arg)
                  (Some(combined), rest2)
          case _ =>
            (Some(node), rest0)

  // ── Grammar: S -> atom | unary(S) | binary(S)(S) | (E) | text  ──────────

  private def parseSexpr(str: String, depth: Int): (Option[MathExpr], String) =
    val s = skipWS(str)
    if s.isEmpty then return (None, s)

    getSymbol(s) match

      case Some(sym) if sym.ttype == RIGHTBRACKET =>
        (None, s)

      case Some(sym) if sym.ttype == INFIX =>
        (None, s)

      case Some(sym) if sym.ttype == DEFINITION =>
        parseSexpr(sym.output + s.substring(sym.input.length), depth)

      case Some(sym) =>
        val rest0 = s.substring(sym.input.length)
        sym.ttype match

          case CONST =>
            val node = if sym.tag == "mi" then Symbol(sym.output) else Operator(sym.output)
            (Some(node), rest0)

          case UNDEROVER =>
            (Some(Operator(sym.output)), rest0)

          case SPACE =>
            val node = if sym.tag == "mtext" then TextNode(sym.output) else Operator(sym.output)
            (Some(node), rest0)

          case LEFTBRACKET =>
            val (innerExprs, rest1) = parseExpr(rest0, rightbracket = false, depth = depth + 1)
            val inner = innerExprs match
              case Nil     => Symbol("")
              case List(e) => e
              case es      => ExprSeq(es)
            val restAfterRB = skipWS(rest1)
            val (rbOpt, rest2) = getSymbol(restAfterRB) match
              case Some(rb) if rb.ttype == RIGHTBRACKET =>
                (Some(rb), restAfterRB.substring(rb.input.length))
              case _ =>
                (None, rest1)
            val finalNode =
              if sym.invisible then inner
              else
                val closeStr = rbOpt.map(_.output).getOrElse("")
                tryBuildMatrix(inner) match
                  case Some(matrix) => BracketGroup(sym.output, closeStr, matrix)
                  case None         => BracketGroup(sym.output, closeStr, inner)
            (Some(finalNode), rest2)

          case TEXTTYPE =>
            val c0 = rest0.headOption.getOrElse('\u0000')
            val (closeChar, startIdx) =
              if c0 == '{'  then ('}',  1)
              else if c0 == '(' then (')', 1)
              else if c0 == '[' then (']', 1)
              else ('\u0000', 0)
            if closeChar != '\u0000' then
              val endIdx = rest0.indexOf(closeChar, startIdx)
              if endIdx >= startIdx then
                (Some(TextNode(rest0.substring(startIdx, endIdx))), rest0.substring(endIdx + 1))
              else
                (Some(TextNode("")), rest0.substring(startIdx))
            else
              (Some(TextNode("")), rest0)

          case UNARY | UNARYUNDEROVER if sym.func =>
            val node = if sym.tag == "mi" then Symbol(sym.output) else Operator(sym.output)
            (Some(node), rest0)

          case UNARY | UNARYUNDEROVER =>
            val (argOpt, rest1) = parseSexpr(rest0, depth)
            argOpt match
              case None =>
                val node = if sym.tag == "mi" then Symbol(sym.output) else Operator(sym.output)
                (Some(node), rest0)
              case Some(arg) =>
                (Some(buildUnary(sym, arg)), rest1)

          case BINARY =>
            val (arg1Opt, rest1) = parseSexpr(rest0, depth)
            val arg1 = stripBrackets(arg1Opt.getOrElse(Symbol("")))
            val (arg2Opt, rest2) = parseSexpr(rest1, depth)
            val arg2 = stripBrackets(arg2Opt.getOrElse(Symbol("")))
            (Some(buildBinary(sym, arg1, arg2)), rest2)

          case LEFTRIGHT =>
            val (innerExprs, rest1) = parseExpr(rest0, rightbracket = true, depth = depth + 1)
            val inner = innerExprs match
              case Nil     => Symbol("")
              case List(e) => e
              case es      => ExprSeq(es)
            val restAfterInner = skipWS(rest1)
            if getSymbol(restAfterInner).exists(_.ttype == LEFTRIGHT) then
              val rb    = getSymbol(restAfterInner).get
              val rest2 = restAfterInner.substring(rb.input.length)
              (Some(BracketGroup("∣", "∣", inner)), rest2)
            else
              (Some(Operator("∣")), rest0)

          case _ =>
            val node = if sym.tag == "mi" then Symbol(sym.output) else Operator(sym.output)
            (Some(node), rest0)

      case None =>
        getNumberOrChar(s) match
          case Some((expr, rest)) => (Some(expr), rest)
          case None               => (None, s)

  // ── Matrix detection helpers ─────────────────────────────────────────────

  private def splitRowByCols(row: MathExpr): List[MathExpr] =
    row match
      case ExprSeq(elems) =>
        val (cols, last) = elems.foldLeft((List.empty[MathExpr], List.empty[MathExpr])) {
          case ((cols, current), Operator(",")) =>
            val cell = current match
              case List(single) => single
              case Nil          => Symbol("")
              case _            => ExprSeq(current)
            (cols :+ cell, Nil)
          case ((cols, current), e) =>
            (cols, current :+ e)
        }
        val lastCell = last match
          case List(single) => single
          case Nil          => Symbol("")
          case _            => ExprSeq(last)
        cols :+ lastCell
      case other => List(other)

  private def tryBuildMatrix(inner: MathExpr): Option[Matrix] =
    inner match
      case ExprSeq(elems) =>
        @annotation.tailrec
        def extractRows(remaining: List[MathExpr], acc: List[MathExpr]): Option[List[MathExpr]] =
          remaining match
            case Nil => Some(acc)
            case BracketGroup("(", ")", rowContent) :: Nil =>
              Some(acc :+ rowContent)
            case BracketGroup("(", ")", rowContent) :: Operator(",") :: rest =>
              extractRows(rest, acc :+ rowContent)
            case _ => None
        extractRows(elems, Nil).flatMap { rows =>
          val rowCells = rows.map(splitRowByCols)
          val numCols  = rowCells.head.length
          // Matrix(elements, rows, cols, rowStride, colStride, offset)
          if rowCells.forall(_.length == numCols) then
            Some(Matrix(rowCells.flatten, rows.length, numCols, numCols, 1, 0))
          else None
        }
      case _ => None

  // ── Unary builder ────────────────────────────────────────────────────────

  private def buildUnary(sym: AMSym, rawArg: MathExpr): MathExpr =
    val arg = stripBrackets(rawArg)
    sym.rewriteleftright match
      case Some((l, r)) => BracketGroup(l, r, arg)
      case None =>
        if sym.input == "sqrt" then Root(None, arg)
        else if sym.input == "cancel" then Enclose("updiagonalstrike", arg)
        else if sym.acc then
          val accent = Operator(sym.output)
          if sym.tag == "mover" then Over(arg, accent) else Under(arg, accent)
        else if sym.tag == "mstyle" then Style(sym.output, arg)
        else Operator(sym.output)

  // ── Binary builder ───────────────────────────────────────────────────────

  private def buildBinary(sym: AMSym, arg1: MathExpr, arg2: MathExpr): MathExpr =
    sym.input match
      case "root"             => Root(Some(arg1), arg2)
      case "frac"             => Fraction(arg1, arg2)
      case "stackrel" | "overset"  => Over(arg2, arg1)
      case "underset"         => Under(arg2, arg1)
      case "color"            => Color(extractText(arg1), arg2)
      case _                  => ExprSeq(List(arg1, arg2))

  // ── Helpers ──────────────────────────────────────────────────────────────

  private def stripBrackets(expr: MathExpr): MathExpr = expr match
    case BracketGroup("(", ")", inner) => inner
    case BracketGroup("[", "]", inner) => inner
    case BracketGroup("{", "}", inner) => inner
    case other                         => other

  private def extractText(expr: MathExpr): String = expr match
    case Symbol(n)    => n
    case Operator(s)  => s
    case Number(v)    => if v % 1 == 0 then v.toLong.toString else v.toString
    case ExprSeq(es)  => es.map(extractText).mkString
    case _            => ""

  private def getNumberOrChar(str: String): Option[(MathExpr, String)] =
    if str.isEmpty then None
    else
      val c = str.head
      if c.isDigit then
        var i = 1
        while i < str.length && str(i).isDigit do i += 1
        if i < str.length && str(i) == '.' then
          var j = i + 1
          while j < str.length && str(j).isDigit do j += 1
          if j > i + 1 then
            Some((Number(str.substring(0, j).toDouble), str.substring(j)))
          else
            Some((Number(str.substring(0, i).toDouble), str.substring(i)))
        else
          Some((Number(str.substring(0, i).toDouble), str.substring(i)))
      else if c.isLetter then
        Some((Symbol(c.toString), str.substring(1)))
      else
        Some((Operator(c.toString), str.substring(1)))
