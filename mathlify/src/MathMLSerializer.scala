package mathlify

import MathExpr.*

/** Serialises a [[MathExpr]] to a MathML string. Works on all platforms (JVM, JS, Native).
  */
object MathMLSerializer:

  private val NS = "http://www.w3.org/1998/Math/MathML"

  private def elem(tag: String, path: String, attrs: (String, String)*)(children: String*): String =
    val attrStr = (s"""data-mathlify-id="$path"""" +: attrs.map((k, v) => s"""$k="$v"""")).mkString(" ")
    val body = children.mkString
    s"<$tag $attrStr>$body</$tag>"
  end elem

  private def mo(symbol: String, path: String): String =
    elem("mo", path)(escapeXml(symbol))

  private def escapeXml(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

  private def prec[A](expr: MathExpr[A]): Int = expr match
    case _: Add[?] | _: Sub[?]      => 1
    case _: Mul[?] | _: Fraction[?] => 2
    case _: Neg[?]                  => 3
    case _: Pow[?]                  => 4
    case _                          => 99

  private def withParens[A](expr: MathExpr[A], minPrec: Int, path: String)(using show: MathShow[A]): String =
    if prec(expr) < minPrec then
      elem("mrow", path)(
        mo("(", s"$path.open"),
        compile(expr, s"$path.inner"),
        mo(")", s"$path.close")
      )
    else compile(expr, path)

  def compile[A](expr: MathExpr[A], path: String = "0")(using show: MathShow[A]): String =
    expr match

      case Number(v) =>
        val str = show.show(v)
        elem("mn", path)(escapeXml(str))

      case Symbol(n) =>
        elem("mi", path)(escapeXml(n))

      case Constant(n) =>
        val display = n match
          case "pi"  => "π"
          case "inf" => "∞"
          case other => other
        elem("mi", path)(escapeXml(display))

      case Add(l, r) =>
        elem("mrow", path)(
          compile(l, s"$path.0"),
          mo("+", s"$path.op"),
          compile(r, s"$path.1")
        )

      case Sub(l, r) =>
        val rStr = r match
          case _: Add[?] | _: Sub[?] => withParens(r, 1, s"$path.1")
          case _                     => compile(r, s"$path.1")
        elem("mrow", path)(
          compile(l, s"$path.0"),
          mo("-", s"$path.op"),
          rStr
        )

      case Mul(l, r) =>
        elem("mrow", path)(
          withParens(l, 2, s"$path.0"),
          withParens(r, 2, s"$path.1")
        )

      case Div(l, r) =>
        elem("mfrac", path)(
          compile(l, s"$path.0"),
          compile(r, s"$path.1")
        )

      case Pow(base, exponent) =>
        val baseStr = base match
          case _: Add[?] | _: Sub[?] | _: Mul[?] | _: Neg[?] | _: Fraction[?] =>
            withParens(base, 99, s"$path.0")
          case _ => compile(base, s"$path.0")
        val expRow = elem("mrow", s"$path.1")(compile(exponent, s"$path.1.0"))
        elem("msup", path)(baseStr, expRow)

      case Neg(e) =>
        val inner = e match
          case _: Add[?] | _: Sub[?] => withParens(e, 1, s"$path.0")
          case _                     => compile(e, s"$path.0")
        elem("mrow", path)(
          mo("-", s"$path.op"),
          inner
        )

      case FunctionCall(name, args) =>
        val nameElem = elem("mi", s"$path.name")(escapeXml(name))
        val argElems = args.zipWithIndex.flatMap { case (arg, i) =>
          val comma = if i > 0 then List(mo(",", s"$path.comma$i")) else Nil
          comma :+ compile(arg, s"$path.$i")
        }
        elem("mrow", path)(
          (List(nameElem, mo("(", s"$path.open")) ++ argElems ++ List(mo(")", s"$path.close")))*
        )

      case Fraction(n, d) =>
        elem("mfrac", path)(
          compile(n, s"$path.0"),
          compile(d, s"$path.1")
        )

      case Root(None, radicand) =>
        elem("msqrt", path)(compile(radicand, s"$path.0"))

      case Root(Some(degree), radicand) =>
        elem("mroot", path)(
          compile(radicand, s"$path.0"),
          compile(degree, s"$path.1")
        )

      case Sum(index, lower, upper, body) =>
        val lowerRow = elem("mrow", s"$path.0.1")(
          compile(index, s"$path.0.1.0"),
          mo("=", s"$path.0.1.op"),
          compile(lower, s"$path.0.1.1")
        )
        val underover = elem("munderover", s"$path.0")(
          mo("∑", s"$path.0.0"),
          lowerRow,
          compile(upper, s"$path.0.2")
        )
        elem("mrow", path)(underover, compile(body, s"$path.1"))

      case Integral(variable, lower, upper, body) =>
        val underover = elem("munderover", s"$path.0")(
          mo("∫", s"$path.0.0"),
          compile(lower, s"$path.0.1"),
          compile(upper, s"$path.0.2")
        )
        elem("mrow", path)(
          underover,
          compile(body, s"$path.1"),
          mo("d", s"$path.d"),
          compile(variable, s"$path.var")
        )

      case Group(e) =>
        elem("mrow", path)(
          mo("(", s"$path.open"),
          compile(e, s"$path.0"),
          mo(")", s"$path.close")
        )

      case MathVector(elements) =>
        val items = elements.zipWithIndex.flatMap { case (el, i) =>
          val comma = if i > 0 then List(mo(",", s"$path.comma$i")) else Nil
          comma :+ compile(el, s"$path.$i")
        }
        elem("mrow", path)(
          (List(mo("(", s"$path.open")) ++ items ++ List(mo(")", s"$path.close")))*
        )

      case Matrix(elements, rows, cols, rowStride, colStride, offset) =>
        val rowStrs = (0 until rows).map { row =>
          val cellStrs = (0 until cols).map { col =>
            val idx = row * rowStride + col * colStride + offset
            elem("mtd", s"$path.$row.$col")(compile(elements(idx), s"$path.$row.$col.0"))
          }
          elem("mtr", s"$path.$row")(cellStrs*)
        }
        elem("mtable", path)(rowStrs*)

      case Subscript(base, sub) =>
        elem("msub", path)(
          compile(base, s"$path.0"),
          compile(sub, s"$path.1")
        )

      case Superscript(base, sup) =>
        elem("msup", path)(
          compile(base, s"$path.0"),
          compile(sup, s"$path.1")
        )

      case Operator(sym) =>
        elem("mo", path)(escapeXml(sym))

      case ExprSeq(exprs) =>
        val items = exprs.zipWithIndex.map { case (e, i) => compile(e, s"$path.$i") }
        elem("mrow", path)(items*)

      case Over(base, top) =>
        elem("mover", path)(
          compile(base, s"$path.0"),
          compile(top, s"$path.1")
        )

      case Under(base, bottom) =>
        elem("munder", path)(
          compile(base, s"$path.0"),
          compile(bottom, s"$path.1")
        )

      case SubSup(base, sub, sup) =>
        elem("msubsup", path)(
          compile(base, s"$path.0"),
          compile(sub, s"$path.1"),
          compile(sup, s"$path.2")
        )

      case Style(variant, content) =>
        elem("mstyle", path, ("mathvariant", variant))(compile(content, s"$path.0"))

      case TextNode(content) =>
        elem("mtext", path)(escapeXml(content))

      case BracketGroup(open, close, content) =>
        val parts =
          (if open.nonEmpty then List(mo(open, s"$path.open")) else Nil) ++
            List(compile(content, s"$path.0")) ++
            (if close.nonEmpty then List(mo(close, s"$path.close")) else Nil)
        elem("mrow", path)(parts*)

      case Enclose(notation, content) =>
        elem("menclose", path, ("notation", notation))(compile(content, s"$path.0"))

      case Color(color, content) =>
        elem("mstyle", path, ("mathcolor", color))(compile(content, s"$path.0"))

  def toMathML[A](expr: MathExpr[A])(using show: MathShow[A]): String =
    s"""<math xmlns="$NS" data-mathlify-id="root">${compile(expr, "0")}</math>"""
end MathMLSerializer
