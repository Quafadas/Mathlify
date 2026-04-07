package mathlify

import MathExpr.*

/** Reverse-mode (backpropagation) automatic differentiation.
  *
  * Builds a computation graph ("tape") during a forward pass, then propagates adjoints backward from the output to
  * compute all partial derivatives in a single reverse sweep.
  *
  * Each node in the tape records:
  *   - the operation performed
  *   - the forward-pass value
  *   - its parent indices
  *   - a human-readable label
  *
  * After the reverse pass every input node holds its adjoint ∂output/∂input.
  */
object ReverseDiff:

  // ── Tape representation ──────────────────────────────────────────────────

  /** The kind of operation that produced a tape node. */
  enum Op:
    case Const // literal constant
    case Var(name: String) // input variable
    case AddOp, SubOp, MulOp, DivOp, PowOp, NegOp
    case SinOp, CosOp, TanOp, ExpOp, LogOp, SqrtOp

  /** A single node in the computation tape. */
  case class TapeNode(
      op: Op,
      value: Double,
      parents: List[Int], // indices of parent nodes
      label: String // human-readable label for display
  )

  /** One step in the reverse (adjoint) propagation — records which node received what adjoint increment. */
  case class BackStep(
      nodeIndex: Int,
      adjointBefore: Double,
      adjointIncrement: Double,
      fromNode: Int, // which output node pushed this adjoint
      rule: String // human-readable description of the rule applied
  )

  /** Complete result of reverse-mode AD. */
  case class ReverseResult(
      tape: Vector[TapeNode],
      adjoints: Array[Double],
      backSteps: List[BackStep],
      value: Double,
      partials: Map[String, Double]
  )

  // ── Forward pass: build tape ──────────────────────────────────────────────

  private class TapeBuilder:
    private val nodes = scala.collection.mutable.ArrayBuffer.empty[TapeNode]

    def add(op: Op, value: Double, parents: List[Int], label: String): Int =
      val idx = nodes.length
      nodes += TapeNode(op, value, parents, label)
      idx

    def result: Vector[TapeNode] = nodes.toVector
  end TapeBuilder

  /** Build the tape by evaluating `expr` under `env`. Returns the tape and the index of the output node. */
  private def buildTape(
      expr: MathExpr[Double],
      env: Map[String, Double]
  ): Either[String, (Vector[TapeNode], Int)] =
    val builder = new TapeBuilder
    val knownFunctions: Set[String] = Set("sin", "cos", "tan", "exp", "log", "sqrt")

    def go(e: MathExpr[Double]): Either[String, Int] = e match
      case Number(v)  => Right(builder.add(Op.Const, v, Nil, v.toFmtString))
      case Constant(name) =>
        val v = name match
          case "pi" | "π" => math.Pi
          case "e"        => math.E
          case other      => return Left(s"Unknown constant: $other")
        Right(builder.add(Op.Const, v, Nil, name))
      case Symbol(name) =>
        env.get(name) match
          case Some(v) => Right(builder.add(Op.Var(name), v, Nil, name))
          case None    => Left(s"Unbound variable: $name")
      case Add(l, r) => binOp(l, r, Op.AddOp, _ + _, "+")
      case Sub(l, r) => binOp(l, r, Op.SubOp, _ - _, "−")
      case Mul(l, r) => binOp(l, r, Op.MulOp, _ * _, "×")
      case Div(l, r) => binOp(l, r, Op.DivOp, _ / _, "÷")
      case Pow(b, exp) =>
        for
          bi <- go(b)
          ei <- go(exp)
        yield
          val bv = builder.result(bi).value
          val ev = builder.result(ei).value
          builder.add(Op.PowOp, math.pow(bv, ev), List(bi, ei), "pow")
      case Neg(inner) =>
        go(inner).map { i =>
          builder.add(Op.NegOp, -builder.result(i).value, List(i), "neg")
        }
      case FunctionCall(name, List(arg)) => unaryFn(arg, name)
      case Fraction(n, d)                => binOp(n, d, Op.DivOp, _ / _, "÷")
      case Root(None, rad)               => unaryFn(rad, "sqrt")
      case Root(Some(deg), rad) =>
        // nth root = rad ^ (1/deg)
        for
          ri <- go(rad)
          di <- go(deg)
        yield
          val rv = builder.result(ri).value
          val dv = builder.result(di).value
          builder.add(Op.PowOp, math.pow(rv, 1.0 / dv), List(ri, di), "root")
      case Group(inner)              => go(inner)
      case BracketGroup(_, _, inner) => go(inner)
      case Superscript(b, exp) =>
        for
          bi <- go(b)
          ei <- go(exp)
        yield
          val bv = builder.result(bi).value
          val ev = builder.result(ei).value
          builder.add(Op.PowOp, math.pow(bv, ev), List(bi, ei), "pow")
      case Subscript(Symbol(base), Number(n)) =>
        val key = s"${base}_${n.round.toInt}"
        env.get(key) match
          case Some(v) => Right(builder.add(Op.Var(key), v, Nil, key))
          case None    => Left(s"Unbound variable: $key")
      case ExprSeq(exprs) => goExprSeq(exprs)
      case other          => Left(s"Unsupported node: ${other.getClass.getSimpleName}")

    def binOp(
        l: MathExpr[Double],
        r: MathExpr[Double],
        op: Op,
        f: (Double, Double) => Double,
        label: String
    ): Either[String, Int] =
      for
        li <- go(l)
        ri <- go(r)
      yield
        val lv = builder.result(li).value
        val rv = builder.result(ri).value
        builder.add(op, f(lv, rv), List(li, ri), label)

    def unaryFn(arg: MathExpr[Double], name: String): Either[String, Int] =
      go(arg).map { i =>
        val v = builder.result(i).value
        val (op, result) = name match
          case "sin"  => (Op.SinOp, math.sin(v))
          case "cos"  => (Op.CosOp, math.cos(v))
          case "tan"  => (Op.TanOp, math.tan(v))
          case "exp"  => (Op.ExpOp, math.exp(v))
          case "log"  => (Op.LogOp, math.log(v))
          case "sqrt" => (Op.SqrtOp, math.sqrt(v))
          case other  => (Op.Const, Double.NaN) // will error
        builder.add(op, result, List(i), name)
      }

    // ── ExprSeq handling (infix parsing with operator precedence) ─────────

    def goExprSeq(exprs: List[MathExpr[Double]]): Either[String, Int] =
      parseAddExpr(exprs).flatMap { case (idx, rest) =>
        if rest.nonEmpty then Left(s"Unexpected trailing elements in expression")
        else Right(idx)
      }

    def parseAddExpr(items: List[MathExpr[Double]]): Either[String, (Int, List[MathExpr[Double]])] =
      parseMulExpr(items).flatMap { case (li, rest) =>
        parseAddRest(li, rest)
      }

    def parseAddRest(left: Int, items: List[MathExpr[Double]]): Either[String, (Int, List[MathExpr[Double]])] =
      items match
        case Operator("+") :: rest =>
          parseMulExpr(rest).flatMap { case (ri, remaining) =>
            val lv = builder.result(left).value
            val rv = builder.result(ri).value
            val idx = builder.add(Op.AddOp, lv + rv, List(left, ri), "+")
            parseAddRest(idx, remaining)
          }
        case Operator("-") :: rest =>
          parseMulExpr(rest).flatMap { case (ri, remaining) =>
            val lv = builder.result(left).value
            val rv = builder.result(ri).value
            val idx = builder.add(Op.SubOp, lv - rv, List(left, ri), "−")
            parseAddRest(idx, remaining)
          }
        case Operator("=") :: rest =>
          parseAddExpr(rest)
        case _ => Right((left, items))

    def parseMulExpr(items: List[MathExpr[Double]]): Either[String, (Int, List[MathExpr[Double]])] =
      parsePrimaryExpr(items).flatMap { case (li, rest) =>
        parseMulRest(li, rest)
      }

    def parseMulRest(left: Int, items: List[MathExpr[Double]]): Either[String, (Int, List[MathExpr[Double]])] =
      items match
        case Operator(op) :: rest if op == "⋅" || op == "×" || op == "*" =>
          parsePrimaryExpr(rest).flatMap { case (ri, remaining) =>
            val lv = builder.result(left).value
            val rv = builder.result(ri).value
            val idx = builder.add(Op.MulOp, lv * rv, List(left, ri), "×")
            parseMulRest(idx, remaining)
          }
        case Operator("/") :: rest =>
          parsePrimaryExpr(rest).flatMap { case (ri, remaining) =>
            val lv = builder.result(left).value
            val rv = builder.result(ri).value
            val idx = builder.add(Op.DivOp, lv / rv, List(left, ri), "÷")
            parseMulRest(idx, remaining)
          }
        // Implicit multiplication: non-operator followed by parseable primary
        case (head :: _) if !head.isInstanceOf[Operator] =>
          parsePrimaryExpr(items).flatMap { case (ri, remaining) =>
            val lv = builder.result(left).value
            val rv = builder.result(ri).value
            val idx = builder.add(Op.MulOp, lv * rv, List(left, ri), "×")
            parseMulRest(idx, remaining)
          }
        case _ => Right((left, items))

    def parsePrimaryExpr(items: List[MathExpr[Double]]): Either[String, (Int, List[MathExpr[Double]])] =
      items match
        case Nil => Left("Unexpected end of expression")
        case Operator("-") :: rest =>
          parsePrimaryExpr(rest).map { case (i, remaining) =>
            val v = builder.result(i).value
            val idx = builder.add(Op.NegOp, -v, List(i), "neg")
            (idx, remaining)
          }
        case Operator(fname) :: (bg @ BracketGroup("(", ")", _)) :: rest if knownFunctions.contains(fname) =>
          go(bg).flatMap { argIdx =>
            val v = builder.result(argIdx).value
            val (op, result) = fname match
              case "sin"  => (Op.SinOp, math.sin(v))
              case "cos"  => (Op.CosOp, math.cos(v))
              case "tan"  => (Op.TanOp, math.tan(v))
              case "exp"  => (Op.ExpOp, math.exp(v))
              case "log"  => (Op.LogOp, math.log(v))
              case "sqrt" => (Op.SqrtOp, math.sqrt(v))
              case _      => (Op.Const, Double.NaN)
            val idx = builder.add(op, result, List(argIdx), fname)
            Right((idx, rest))
          }
        case Symbol(fname) :: (bg @ BracketGroup("(", ")", _)) :: rest if knownFunctions.contains(fname) =>
          go(bg).flatMap { argIdx =>
            val v = builder.result(argIdx).value
            val (op, result) = fname match
              case "sin"  => (Op.SinOp, math.sin(v))
              case "cos"  => (Op.CosOp, math.cos(v))
              case "tan"  => (Op.TanOp, math.tan(v))
              case "exp"  => (Op.ExpOp, math.exp(v))
              case "log"  => (Op.LogOp, math.log(v))
              case "sqrt" => (Op.SqrtOp, math.sqrt(v))
              case _      => (Op.Const, Double.NaN)
            val idx = builder.add(op, result, List(argIdx), fname)
            Right((idx, rest))
          }
        case expr :: rest =>
          go(expr).map(idx => (idx, rest))

    val prepared = Evaluator.foldConstants(Evaluator.substituteConstantsPublic(expr))
    val result = go(prepared).map(outputIdx => (builder.result, outputIdx))
    result
  end buildTape

  // ── Reverse pass: propagate adjoints ──────────────────────────────────────

  /** Propagate adjoints from output to inputs, recording each step. */
  private def reversePass(tape: Vector[TapeNode], outputIdx: Int): (Array[Double], List[BackStep]) =
    val adjoints = Array.fill(tape.length)(0.0)
    // ∂f/∂f = 1: the output's adjoint is 1 because the derivative of the output with respect to itself is 1
    adjoints(outputIdx) = 1.0

    val steps = scala.collection.mutable.ListBuffer.empty[BackStep]

    // Walk backwards from the output node
    for i <- outputIdx to 0 by -1 do
      val node = tape(i)
      val adj = adjoints(i)
      if adj != 0.0 then
        node.op match
          case Op.Const | _: Op.Var => // leaf nodes — no parents to propagate to

          case Op.AddOp =>
            // d(a+b)/da = 1, d(a+b)/db = 1
            val List(li, ri) = node.parents: @unchecked
            pushAdj(steps, adjoints, li, adj, i, "∂(a+b)/∂a = 1")
            pushAdj(steps, adjoints, ri, adj, i, "∂(a+b)/∂b = 1")

          case Op.SubOp =>
            // d(a-b)/da = 1, d(a-b)/db = -1
            val List(li, ri) = node.parents: @unchecked
            pushAdj(steps, adjoints, li, adj, i, "∂(a−b)/∂a = 1")
            pushAdj(steps, adjoints, ri, -adj, i, "∂(a−b)/∂b = −1")

          case Op.MulOp =>
            // d(a*b)/da = b, d(a*b)/db = a
            val List(li, ri) = node.parents: @unchecked
            val lv = tape(li).value
            val rv = tape(ri).value
            pushAdj(steps, adjoints, li, adj * rv, i, s"∂(a×b)/∂a = b = ${rv.toFmtString}")
            pushAdj(steps, adjoints, ri, adj * lv, i, s"∂(a×b)/∂b = a = ${lv.toFmtString}")

          case Op.DivOp =>
            // d(a/b)/da = 1/b, d(a/b)/db = -a/b²
            val List(li, ri) = node.parents: @unchecked
            val lv = tape(li).value
            val rv = tape(ri).value
            pushAdj(steps, adjoints, li, adj / rv, i, s"∂(a÷b)/∂a = 1/b = ${(1.0 / rv).toFmtString}")
            pushAdj(steps, adjoints, ri, -adj * lv / (rv * rv), i, s"∂(a÷b)/∂b = −a/b² = ${(-lv / (rv * rv)).toFmtString}")

          case Op.PowOp =>
            // d(a^b)/da = b*a^(b-1), d(a^b)/db = a^b * ln(a)
            val List(bi, ei) = node.parents: @unchecked
            val bv = tape(bi).value
            val ev = tape(ei).value
            val powResult = node.value
            pushAdj(steps, adjoints, bi, adj * ev * math.pow(bv, ev - 1), i, s"∂(aᵇ)/∂a = b·a^(b−1)")
            if tape(ei).op != Op.Const then // Constant exponents have no gradient — they are not variables being differentiated
              pushAdj(steps, adjoints, ei, adj * powResult * math.log(bv), i, s"∂(aᵇ)/∂b = aᵇ·ln(a)")

          case Op.NegOp =>
            val List(pi) = node.parents: @unchecked
            pushAdj(steps, adjoints, pi, -adj, i, "∂(−a)/∂a = −1")

          case Op.SinOp =>
            val List(pi) = node.parents: @unchecked
            val pv = tape(pi).value
            pushAdj(steps, adjoints, pi, adj * math.cos(pv), i, s"∂sin(a)/∂a = cos(a) = ${math.cos(pv).toFmtString}")

          case Op.CosOp =>
            val List(pi) = node.parents: @unchecked
            val pv = tape(pi).value
            pushAdj(steps, adjoints, pi, -adj * math.sin(pv), i, s"∂cos(a)/∂a = −sin(a) = ${(-math.sin(pv)).toFmtString}")

          case Op.TanOp =>
            val List(pi) = node.parents: @unchecked
            val pv = tape(pi).value
            val c = math.cos(pv)
            pushAdj(steps, adjoints, pi, adj / (c * c), i, s"∂tan(a)/∂a = 1/cos²(a)")

          case Op.ExpOp =>
            val List(pi) = node.parents: @unchecked
            pushAdj(steps, adjoints, pi, adj * node.value, i, s"∂exp(a)/∂a = exp(a) = ${node.value.toFmtString}")

          case Op.LogOp =>
            val List(pi) = node.parents: @unchecked
            val pv = tape(pi).value
            pushAdj(steps, adjoints, pi, adj / pv, i, s"∂log(a)/∂a = 1/a = ${(1.0 / pv).toFmtString}")

          case Op.SqrtOp =>
            val List(pi) = node.parents: @unchecked
            pushAdj(steps, adjoints, pi, adj / (2.0 * node.value), i, s"∂√a/∂a = 1/(2√a) = ${(1.0 / (2.0 * node.value)).toFmtString}")
    end for
    (adjoints, steps.toList)
  end reversePass

  private def pushAdj(
      steps: scala.collection.mutable.ListBuffer[BackStep],
      adjoints: Array[Double],
      targetIdx: Int,
      increment: Double,
      fromIdx: Int,
      rule: String
  ): Unit =
    val before = adjoints(targetIdx)
    adjoints(targetIdx) += increment
    steps += BackStep(targetIdx, before, increment, fromIdx, rule)

  // ── Public API ────────────────────────────────────────────────────────────

  /** Compute value and all partial derivatives using reverse-mode AD.
    *
    * Returns the full computation tape, adjoints, backward steps for visualization, the function value and a map of
    * partial derivatives.
    */
  def reverseGradient(
      expr: MathExpr[Double],
      env: Map[String, Double]
  ): Either[String, ReverseResult] =
    buildTape(expr, env).map { case (tape, outputIdx) =>
      val (adjoints, backSteps) = reversePass(tape, outputIdx)
      val partials = tape.zipWithIndex
        .collect { case (TapeNode(Op.Var(name), _, _, _), idx) =>
          name -> adjoints(idx)
        }
        .groupMap(_._1)(_._2)
        .map { case (name, adjs) => name -> adjs.sum }
      ReverseResult(tape, adjoints, backSteps, tape(outputIdx).value, partials)
    }

  /** Convenience: compute just the value and partials (like ForwardDiff.gradient). */
  def gradient(
      expr: MathExpr[Double],
      env: Map[String, Double]
  ): Either[String, ForwardDiff.DiffResult] =
    reverseGradient(expr, env).map(r => ForwardDiff.DiffResult(r.value, r.partials))

  /** Format helper for doubles. */
  extension (d: Double)
    private def toFmtString: String =
      if d == d.toLong.toDouble && !d.isInfinite then d.toLong.toString
      else f"$d%.6g"

end ReverseDiff
