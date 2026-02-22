package mathlify

/** Thrown when the input string cannot be parsed as a mathematical expression. */
final class MathParseException(message: String) extends Exception(message)

/** Recursive-descent parser for simple mathematical expressions.
  *
  * Grammar:
  * {{{
  *   equation ::= expr ('=' expr)*
  *   expr     ::= term (('+' | '-') term)*
  *   term     ::= factor (('*' | '/') factor)*
  *   factor   ::= '(' expr ')' | NUM | IDENT
  * }}}
  */
object MathParser:

  private enum Token derives CanEqual:
    case Ident(name: String)
    case Num(value: String)
    case Plus, Minus, Star, Slash, Equals
    case LParen, RParen
    case EOF

  private def tokenize(input: String): Array[Token] =
    val buf = Array.newBuilder[Token]
    var i   = 0
    while i < input.length do
      input(i) match
        case c if c.isWhitespace => i += 1
        case '+' => buf += Token.Plus; i += 1
        case '-' => buf += Token.Minus; i += 1
        case '*' => buf += Token.Star; i += 1
        case '/' => buf += Token.Slash; i += 1
        case '=' => buf += Token.Equals; i += 1
        case '(' => buf += Token.LParen; i += 1
        case ')' => buf += Token.RParen; i += 1
        case c if c.isLetter =>
          val j = i
          i += 1
          while i < input.length && (input(i).isLetterOrDigit || input(i) == '_') do i += 1
          buf += Token.Ident(input.substring(j, i))
        case c if c.isDigit =>
          val j = i
          i += 1
          while i < input.length && (input(i).isDigit || input(i) == '.') do i += 1
          buf += Token.Num(input.substring(j, i))
        case c =>
          throw MathParseException(s"Unexpected character '$c' at position $i")
    buf += Token.EOF
    buf.result()

  def parse(input: String): MathExpr =
    val tokens = tokenize(input.trim)
    var pos    = 0

    def peek: Token = tokens(pos)

    def advance(): Token =
      val t = peek
      if t != Token.EOF then pos += 1
      t

    def expect(t: Token): Unit =
      if peek != t then throw MathParseException(s"Expected $t but got $peek")
      advance()

    // equation ::= expr ('=' expr)*   (right-associative)
    def parseEquation(): MathExpr =
      val lhs = parseExpr()
      if peek == Token.Equals then
        advance()
        MathExpr.Eq(lhs, parseEquation())
      else lhs

    // expr ::= term (('+' | '-') term)*
    def parseExpr(): MathExpr =
      var e = parseTerm()
      while peek == Token.Plus || peek == Token.Minus do
        val op  = advance()
        val rhs = parseTerm()
        e = if op == Token.Plus then MathExpr.Add(e, rhs) else MathExpr.Sub(e, rhs)
      e

    // term ::= factor (('*' | '/') factor)*
    def parseTerm(): MathExpr =
      var e = parseFactor()
      while peek == Token.Star || peek == Token.Slash do
        val op  = advance()
        val rhs = parseFactor()
        e = if op == Token.Star then MathExpr.Mul(e, rhs) else MathExpr.Div(e, rhs)
      e

    // factor ::= '(' expr ')' | NUM | IDENT
    def parseFactor(): MathExpr =
      peek match
        case Token.LParen =>
          advance()
          val inner = parseExpr()
          expect(Token.RParen)
          MathExpr.Parens(inner)
        case Token.Ident(name) =>
          advance()
          MathExpr.Var(name)
        case Token.Num(value) =>
          advance()
          MathExpr.Num(value)
        case Token.EOF =>
          throw MathParseException("Unexpected end of input")
        case t =>
          throw MathParseException(s"Unexpected token: $t")

    val result = parseEquation()
    if peek != Token.EOF then
      throw MathParseException(s"Unexpected token at position $pos: $peek")
    result
