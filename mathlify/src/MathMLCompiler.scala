package mathlify

import org.scalajs.dom
import org.scalajs.dom.Element
import MathExpr.*

object MathMLCompiler:

  private val NS = "http://www.w3.org/1998/Math/MathML"

  private def elem(tag: String, path: String): Element =
    val e = dom.document.createElementNS(NS, tag)
    e.setAttribute("data-mathlify-id", path)
    e

  private def moElem(symbol: String, path: String): Element =
    val e = elem("mo", path)
    e.appendChild(dom.document.createTextNode(symbol))
    e

  private def prec(expr: MathExpr): Int = expr match
    case _: Add | _: Sub           => 1
    case _: Mul | _: Fraction      => 2
    case _: Neg                    => 3
    case _: Pow                    => 4
    case _                         => 99

  private def withParens(expr: MathExpr, minPrec: Int, path: String): Element =
    if prec(expr) < minPrec then
      val row = elem("mrow", path)
      row.appendChild(moElem("(", s"$path.open"))
      row.appendChild(compile(expr, s"$path.inner"))
      row.appendChild(moElem(")", s"$path.close"))
      row
    else
      compile(expr, path)

  def compile(expr: MathExpr, path: String = "0"): Element =
    expr match

      case Number(v) =>
        val e = elem("mn", path)
        val str = if v % 1 == 0 && !v.isInfinite then v.toLong.toString else v.toString
        e.appendChild(dom.document.createTextNode(str))
        e

      case Symbol(n) =>
        val e = elem("mi", path)
        e.appendChild(dom.document.createTextNode(n))
        e

      case Constant(n) =>
        val e = elem("mi", path)
        val display = n match
          case "pi"  => "π"
          case "inf" => "∞"
          case other => other
        e.appendChild(dom.document.createTextNode(display))
        e

      case Add(l, r) =>
        val row = elem("mrow", path)
        row.appendChild(compile(l, s"$path.0"))
        row.appendChild(moElem("+", s"$path.op"))
        row.appendChild(compile(r, s"$path.1"))
        row

      case Sub(l, r) =>
        val row = elem("mrow", path)
        row.appendChild(compile(l, s"$path.0"))
        row.appendChild(moElem("-", s"$path.op"))
        // r needs parens if it's Add or Sub
        val rElem = r match
          case _: Add | _: Sub => withParens(r, 1, s"$path.1")
          case _               => compile(r, s"$path.1")
        row.appendChild(rElem)
        row

      case Mul(l, r) =>
        val row = elem("mrow", path)
        row.appendChild(withParens(l, 2, s"$path.0"))
        row.appendChild(withParens(r, 2, s"$path.1"))
        row

      case Div(l, r) =>
        val frac = elem("mfrac", path)
        frac.appendChild(compile(l, s"$path.0"))
        frac.appendChild(compile(r, s"$path.1"))
        frac

      case Pow(base, exponent) =>
        val msup = elem("msup", path)
        // base needs parens if Add/Sub/Mul/Neg/Fraction
        val baseElem = base match
          case _: Add | _: Sub | _: Mul | _: Neg | _: Fraction => withParens(base, 99, s"$path.0")
          case _                                                 => compile(base, s"$path.0")
        msup.appendChild(baseElem)
        // exponent in an mrow
        val expRow = elem("mrow", s"$path.1")
        expRow.appendChild(compile(exponent, s"$path.1.0"))
        msup.appendChild(expRow)
        msup

      case Neg(e) =>
        val row = elem("mrow", path)
        row.appendChild(moElem("-", s"$path.op"))
        val inner = e match
          case _: Add | _: Sub => withParens(e, 1, s"$path.0")
          case _               => compile(e, s"$path.0")
        row.appendChild(inner)
        row

      case FunctionCall(name, args) =>
        val row = elem("mrow", path)
        val mi  = elem("mi", s"$path.name")
        mi.appendChild(dom.document.createTextNode(name))
        row.appendChild(mi)
        row.appendChild(moElem("(", s"$path.open"))
        args.zipWithIndex.foreach { case (arg, i) =>
          if i > 0 then row.appendChild(moElem(",", s"$path.comma$i"))
          row.appendChild(compile(arg, s"$path.$i"))
        }
        row.appendChild(moElem(")", s"$path.close"))
        row

      case Fraction(n, d) =>
        val frac = elem("mfrac", path)
        frac.appendChild(compile(n, s"$path.0"))
        frac.appendChild(compile(d, s"$path.1"))
        frac

      case Root(None, radicand) =>
        val msqrt = elem("msqrt", path)
        msqrt.appendChild(compile(radicand, s"$path.0"))
        msqrt

      case Root(Some(degree), radicand) =>
        val mroot = elem("mroot", path)
        mroot.appendChild(compile(radicand, s"$path.0"))
        mroot.appendChild(compile(degree, s"$path.1"))
        mroot

      case Sum(index, lower, upper, body) =>
        val row       = elem("mrow", path)
        val underover = elem("munderover", s"$path.0")
        underover.appendChild(moElem("∑", s"$path.0.0"))
        // Lower: <mrow>index = lower</mrow>
        val lowerRow = elem("mrow", s"$path.0.1")
        lowerRow.appendChild(compile(index, s"$path.0.1.0"))
        lowerRow.appendChild(moElem("=", s"$path.0.1.op"))
        lowerRow.appendChild(compile(lower, s"$path.0.1.1"))
        underover.appendChild(lowerRow)
        underover.appendChild(compile(upper, s"$path.0.2"))
        row.appendChild(underover)
        row.appendChild(compile(body, s"$path.1"))
        row

      case Integral(variable, lower, upper, body) =>
        val row       = elem("mrow", path)
        val underover = elem("munderover", s"$path.0")
        underover.appendChild(moElem("∫", s"$path.0.0"))
        underover.appendChild(compile(lower, s"$path.0.1"))
        underover.appendChild(compile(upper, s"$path.0.2"))
        row.appendChild(underover)
        row.appendChild(compile(body, s"$path.1"))
        row.appendChild(moElem("d", s"$path.d"))
        row.appendChild(compile(variable, s"$path.var"))
        row

      case Group(e) =>
        val row = elem("mrow", path)
        row.appendChild(moElem("(", s"$path.open"))
        row.appendChild(compile(e, s"$path.0"))
        row.appendChild(moElem(")", s"$path.close"))
        row

      case MathVector(elements) =>
        val row = elem("mrow", path)
        row.appendChild(moElem("(", s"$path.open"))
        elements.zipWithIndex.foreach { case (el, i) =>
          if i > 0 then row.appendChild(moElem(",", s"$path.comma$i"))
          row.appendChild(compile(el, s"$path.$i"))
        }
        row.appendChild(moElem(")", s"$path.close"))
        row

      case Matrix(elements, rows, cols, rowStride, colStride, offset) =>
        val table = elem("mtable", path)
        for row <- 0 until rows do
          val mtr = elem("mtr", s"$path.$row")
          for col <- 0 until cols do
            val mtd = elem("mtd", s"$path.$row.$col")
            val idx = row * rowStride + col * colStride + offset
            mtd.appendChild(compile(elements(idx), s"$path.$row.$col.0"))
            mtr.appendChild(mtd)
          table.appendChild(mtr)
        table

      case Subscript(base, sub) =>
        val msub = elem("msub", path)
        msub.appendChild(compile(base, s"$path.0"))
        msub.appendChild(compile(sub, s"$path.1"))
        msub

      case Superscript(base, sup) =>
        val msup = elem("msup", path)
        msup.appendChild(compile(base, s"$path.0"))
        msup.appendChild(compile(sup, s"$path.1"))
        msup

      case Operator(sym) =>
        val e = elem("mo", path)
        e.appendChild(dom.document.createTextNode(sym))
        e

      case ExprSeq(exprs) =>
        val row = elem("mrow", path)
        exprs.zipWithIndex.foreach { case (e, i) =>
          row.appendChild(compile(e, s"$path.$i"))
        }
        row

      case Over(base, top) =>
        val mover = elem("mover", path)
        mover.appendChild(compile(base, s"$path.0"))
        mover.appendChild(compile(top, s"$path.1"))
        mover

      case Under(base, bottom) =>
        val munder = elem("munder", path)
        munder.appendChild(compile(base, s"$path.0"))
        munder.appendChild(compile(bottom, s"$path.1"))
        munder

      case SubSup(base, sub, sup) =>
        val msubsup = elem("msubsup", path)
        msubsup.appendChild(compile(base, s"$path.0"))
        msubsup.appendChild(compile(sub, s"$path.1"))
        msubsup.appendChild(compile(sup, s"$path.2"))
        msubsup

      case Style(variant, content) =>
        val mstyle = elem("mstyle", path)
        mstyle.setAttribute("mathvariant", variant)
        mstyle.appendChild(compile(content, s"$path.0"))
        mstyle

      case TextNode(content) =>
        val e = elem("mtext", path)
        e.appendChild(dom.document.createTextNode(content))
        e

      case BracketGroup(open, close, content) =>
        val row = elem("mrow", path)
        if open.nonEmpty  then row.appendChild(moElem(open,  s"$path.open"))
        row.appendChild(compile(content, s"$path.0"))
        if close.nonEmpty then row.appendChild(moElem(close, s"$path.close"))
        row

      case Enclose(notation, content) =>
        val menclose = elem("menclose", path)
        menclose.setAttribute("notation", notation)
        menclose.appendChild(compile(content, s"$path.0"))
        menclose

      case Color(color, content) =>
        val mstyle = elem("mstyle", path)
        mstyle.setAttribute("mathcolor", color)
        mstyle.appendChild(compile(content, s"$path.0"))
        mstyle

  def toMathML(expr: MathExpr): Element =
    val math = dom.document.createElementNS(NS, "math")
    math.setAttribute("data-mathlify-id", "root")
    math.appendChild(compile(expr, "0"))
    math
