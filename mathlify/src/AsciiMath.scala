package mathlify

import MathExpr.*

object AsciiMath:

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
      case Nil     => Symbol("")
      case List(e) => e
      case es      => ExprSeq(es))
  end translate

  // ── Grammar: E -> (I (/I)?)*  ─────────────────────────────────────────────

  private def parseExpr(
      str: String,
      rightbracket: Boolean,
      depth: Int
  ): (List[MathExpr], String) =
    var current = str
    var result = List.empty[MathExpr]
    var continue = true
    while continue do
      current = skipWS(current)
      val sym = getSymbol(current)
      if current.isEmpty
        || sym.exists(s => s.ttype == RIGHTBRACKET && depth > 0)
        || sym.exists(s => s.ttype == LEFTRIGHT && rightbracket)
      then continue = false
      else
        val (nodeOpt, rest0) = parseIexpr(current, depth)
        nodeOpt match
          case None =>
            continue = false
          case Some(node) =>
            val s1 = skipWS(rest0)
            val fracSym = getSymbol(s1)
            if fracSym.exists(s => s.ttype == INFIX && s.input == "/") then
              val rest1 = skipWS(s1.substring(1))
              val (arg2Opt, rest2) = parseIexpr(rest1, depth)
              val arg2 = arg2Opt.getOrElse(Operator("□"))
              result = result :+ Fraction(stripBrackets(node), stripBrackets(arg2))
              current = rest2
            else
              result = result :+ node
              current = rest0
            end if
        end match
      end if
    end while
    (result, current)
  end parseExpr

  // ── Grammar: I -> S (_ S (^ S)? | ^ S (_ S)?)?  ─────────────────────────

  private def parseIexpr(str: String, depth: Int): (Option[MathExpr], String) =
    val sym1Opt = getSymbol(skipWS(str))
    val (nodeOpt, rest0) = parseSexpr(str, depth)
    nodeOpt match
      case None       => (None, rest0)
      case Some(node) =>
        val s1 = skipWS(rest0)
        val sym = getSymbol(s1)
        sym match
          case Some(infix) if infix.ttype == INFIX && infix.input != "/" =>
            val rest1 = skipWS(s1.substring(infix.input.length))
            val (argOpt, rest2) = parseSexpr(rest1, depth)
            val arg = stripBrackets(argOpt.getOrElse(Operator("□")))
            val isUO = sym1Opt.exists(s => s.ttype == UNDEROVER || s.ttype == UNARYUNDEROVER)
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
              end match
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
              end match
            end if
          case _ =>
            (Some(node), rest0)
        end match
    end match
  end parseIexpr

  // ── Grammar: S -> atom | unary(S) | binary(S)(S) | (E) | text  ──────────

  private def parseSexpr(str: String, depth: Int): (Option[MathExpr], String) =
    val s = skipWS(str)
    if s.isEmpty then return (None, s)
    end if

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
                end match
            (Some(finalNode), rest2)

          case TEXTTYPE =>
            val c0 = rest0.headOption.getOrElse('\u0000')
            val (closeChar, startIdx) =
              if c0 == '{' then ('}', 1)
              else if c0 == '(' then (')', 1)
              else if c0 == '[' then (']', 1)
              else ('\u0000', 0)
            if closeChar != '\u0000' then
              val endIdx = rest0.indexOf(closeChar, startIdx)
              if endIdx >= startIdx then (Some(TextNode(rest0.substring(startIdx, endIdx))), rest0.substring(endIdx + 1))
              else (Some(TextNode("")), rest0.substring(startIdx))
              end if
            else (Some(TextNode("")), rest0)
            end if

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
            end match

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
              val rb = getSymbol(restAfterInner).get
              val rest2 = restAfterInner.substring(rb.input.length)
              (Some(BracketGroup("∣", "∣", inner)), rest2)
            else (Some(Operator("∣")), rest0)
            end if

          case _ =>
            val node = if sym.tag == "mi" then Symbol(sym.output) else Operator(sym.output)
            (Some(node), rest0)
        end match

      case None =>
        getNumberOrChar(s) match
          case Some((expr, rest)) => (Some(expr), rest)
          case None               => (None, s)
    end match
  end parseSexpr

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
        def extractRows(
            open: String,
            close: String,
            remaining: List[MathExpr],
            acc: List[MathExpr]
        ): Option[List[MathExpr]] =
          remaining match
            case Nil                                              => Some(acc)
            case BracketGroup(`open`, `close`, rowContent) :: Nil =>
              Some(acc :+ rowContent)
            case BracketGroup(`open`, `close`, rowContent) :: Operator(",") :: rest =>
              extractRows(open, close, rest, acc :+ rowContent)
            case _ => None
        val rowsOpt =
          extractRows("(", ")", elems, Nil)
            .orElse(extractRows("[", "]", elems, Nil))
        rowsOpt.flatMap { rows =>
          val rowCells = rows.map(splitRowByCols)
          val numCols = rowCells.head.length
          // Matrix(elements, rows, cols, rowStride, colStride, offset)
          if rowCells.forall(_.length == numCols) then Some(Matrix(rowCells.flatten, rows.length, numCols, numCols, 1, 0))
          else None
          end if
        }
      case _ => None

  // ── Unary builder ────────────────────────────────────────────────────────

  private def buildUnary(sym: AMSym, rawArg: MathExpr): MathExpr =
    val arg = stripBrackets(rawArg)
    sym.rewriteleftright match
      case Some((l, r)) => BracketGroup(l, r, arg)
      case None         =>
        if sym.input == "sqrt" then Root(None, arg)
        else if sym.input == "cancel" then Enclose("updiagonalstrike", arg)
        else if sym.acc then
          val accent = Operator(sym.output)
          if sym.tag == "mover" then Over(arg, accent) else Under(arg, accent)
          end if
        else if sym.tag == "mstyle" then Style(sym.output, arg)
        else Operator(sym.output)
    end match
  end buildUnary

  // ── Binary builder ───────────────────────────────────────────────────────

  private def buildBinary(sym: AMSym, arg1: MathExpr, arg2: MathExpr): MathExpr =
    sym.input match
      case "root"                 => Root(Some(arg1), arg2)
      case "frac"                 => Fraction(arg1, arg2)
      case "stackrel" | "overset" => Over(arg2, arg1)
      case "underset"             => Under(arg2, arg1)
      case "color"                => Color(extractText(arg1), arg2)
      case _                      => ExprSeq(List(arg1, arg2))

  // ── Helpers ──────────────────────────────────────────────────────────────

  private def stripBrackets(expr: MathExpr): MathExpr = expr match
    case BracketGroup("(", ")", inner) => inner
    case BracketGroup("[", "]", inner) => inner
    case BracketGroup("{", "}", inner) => inner
    case other                         => other

  private def extractText(expr: MathExpr): String = expr match
    case Symbol(n)   => n
    case Operator(s) => s
    case Number(v)   => if v % 1 == 0 then v.toLong.toString else v.toString
    case ExprSeq(es) => es.map(extractText).mkString
    case _           => ""

  private def getNumberOrChar(str: String): Option[(MathExpr, String)] =
    if str.isEmpty then None
    else
      val c = str.head
      if c.isDigit then
        var i = 1
        while i < str.length && str(i).isDigit do i += 1
        end while
        if i < str.length && str(i) == '.' then
          var j = i + 1
          while j < str.length && str(j).isDigit do j += 1
          end while
          if j > i + 1 then Some((Number(str.substring(0, j).toDouble), str.substring(j)))
          else Some((Number(str.substring(0, i).toDouble), str.substring(i)))
          end if
        else Some((Number(str.substring(0, i).toDouble), str.substring(i)))
        end if
      else if c.isLetter then Some((Symbol(c.toString), str.substring(1)))
      else Some((Operator(c.toString), str.substring(1)))
      end if
end AsciiMath
