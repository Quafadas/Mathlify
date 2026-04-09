# Mathlify – Integration Design Document

## 1. What Is Mathlify?

Mathlify is a **cross-platform Scala 3 library** (JVM, Scala.js, Scala Native) that parses plain-text mathematical expressions into a typed AST, evaluates them numerically, and can serialise to MathML for display on JS.

The library is published under the `io.github.quafadas` organisation as `mathlify`. It has a minimal dependency footprint: only `fastparse` on the core, plus Laminar/scalajs-dom for the js variant.

---

## 2. High-Level Capabilities

### 2.1 Parsing

| Capability | Entry Point | Details |
|---|---|---|
| **AsciiMath → AST** | `AsciiMath.translate(input): Either[String, MathExpr[Double]]` | Full AsciiMath grammar: Greek letters, operators, brackets, subscripts, superscripts, fractions (`/`), roots, sums, integrals, text nodes, colour/style annotations, vectors, and matrices. |
| **Simple math string → AST** | `MathParser.parse(input): Either[String, MathExpr[Double]]` | Lighter-weight parser using fastparse. Supports numbers, symbols, `+`, `-`, `*`, `/`, `^`, parentheses, function calls (`sin`, `cos`, `exp`, `log`, `sqrt`), subscripts, sums, integrals. |

Both parsers produce the same `MathExpr[A]` AST.

### 2.2 Expression AST (`MathExpr[A]`)

The AST is a **sealed trait parameterised on the numeric type `A`**. This is the central data structure—everything flows through it.

**Node categories:**

| Category | Nodes |
|---|---|
| Atoms | `Number[A]`, `Symbol`, `Constant` |
| Arithmetic | `Add`, `Sub`, `Mul`, `Div`, `Pow`, `Neg` |
| Structures | `FunctionCall`, `Fraction`, `Root`, `Sum`, `Integral`, `Group` |
| Collections | `MathVector`, `Matrix` |
| Annotations | `Subscript`, `Superscript`, `SubSup`, `Over`, `Under` |
| AsciiMath-specific | `Operator`, `ExprSeq`, `BracketGroup`, `Style`, `TextNode`, `Enclose`, `Color` |

**Key design decisions:**

- **Generic in `A`**: Operations are parameterised so the same AST works with `Double` or any type with the right type class instance — it is an intentional extension point.
- **Covariant (`+A`)**: Leaf nodes like `Symbol` and `Constant` extend `MathExpr[Nothing]` and work in any context.

### 2.3 Evaluation

| Capability | Entry Point | Details |
|---|---|---|
| **Full eval (Double)** | `Evaluator.eval(expr, env)` | Folds constants, substitutes known constants (π, e), evaluates with provided variable bindings. Returns `EvalResult[Double]`. |
| **Generic eval** | `Evaluator.eval[A](expr, env)(using MathTrig[A])` | Same logic, works for any type `A` with a `MathTrig[A]` instance. |
| **Partial eval** | `Evaluator.partialEval(expr, env)` | Evaluates what it can, returns `PartiallyReduced(expr)` if unbound variables remain. |
| **Constant folding** | `Evaluator.foldConstants(expr)` | Simplifies sub-expressions where both sides are numeric (Double only). |
| **Free variable analysis** | `Evaluator.freeVars(expr)` | Returns `Set[String]` of all unbound variable names. |
| **Evaluability check** | `Evaluator.isEvaluable(expr, env)` | Boolean: can the expression be fully evaluated with the given environment? |
| **Parse-and-eval shortcut** | `Evaluator.parseConstant(input)` | AsciiMath → `Option[Double]` in one call. |

**Result type:**

```
sealed trait EvalResult[+A]
  case class Numeric[A](value: A)
  case class PartiallyReduced[A](expr: MathExpr[A])
  case class EvalError(message: String)
```

### 2.4 Type Class Algebra

The evaluator is generic over a **type class hierarchy**:

```
MathRing[A]   — zero, one, fromLong, fromDouble, plus, minus, times, negate
  └─ MathField[A]   — + div
       └─ MathPow[A]     — + pow
            └─ MathTrig[A]     — + sin, cos, tan, exp, log, sqrt
```

A `given MathTrig[Double]` is provided out of the box. To evaluate over a custom numeric type, provide a `MathTrig[YourType]` instance.

`MathShow[A]` controls display formatting (e.g. `3.0` renders as `"3"` for `Double`).

### 2.5 MathML Serialisation

| Capability | Entry Point | Platform |
|---|---|---|
| **String MathML** | `MathMLSerializer.compile(expr): String` | All (JVM, JS, Native) |
| **DOM MathML** | `MathMLCompiler.compile(expr): dom.Element` | JS only |
| **Laminar rendering** | `LaminarRenderer.render(expr): HtmlElement` | JS only |
| **Reactive rendering** | `LaminarRenderer.render(signal): HtmlElement` | JS only (takes `Signal[MathExpr[Double]]`) |
| **Inline rendering** | `LaminarRenderer.renderInline(expr): HtmlElement` | JS only (produces `<span>`) |

All serialised MathML nodes carry `data-mathlify-id` path attributes for DOM identification and testing.

---

## 3. Platform Matrix

| Feature | JVM | Scala.js | Scala Native |
|---|:---:|:---:|:---:|
| AST (`MathExpr`) | ✓ | ✓ | ✓ |
| `MathParser` | ✓ | ✓ | ✓ |
| `AsciiMath` | ✓ | ✓ | ✓ |
| `Evaluator` | ✓ | ✓ | ✓ |
| `MathMLSerializer` (String) | ✓ | ✓ | ✓ |
| `MathMLCompiler` (DOM) | — | ✓ | — |
| `LaminarRenderer` | — | ✓ | — |

---

## 4. Dependencies

| Dependency | Version | Used By | Platform |
|---|---|---|---|
| `com.lihaoyi::fastparse` | 3.1.1 | `MathParser`, `AsciiMath` | All |
| `com.raquo::laminar` | 18.0.0-M5 | `LaminarRenderer` | JS only |
| `org.scala-js::scalajs-dom` | 2.2.0 | `MathMLCompiler`, `LaminarRenderer` | JS only |

The core AST, AsciiMath parser, evaluator, and string MathML serialiser depend only on `fastparse`. The Laminar/DOM layer is additive and only present in the JS variant.

---

## 5. Integration Boundaries

### 5.1 Consuming the Library

An upstream library should depend on one of:

| Use Case | Depend On | You Get |
|---|---|---|
| JVM server-side math processing | `mathlify.jvm` | Parse, eval, fold, MathML strings |
| JS app without Laminar | `mathlify.js` | Parse, eval, fold, MathML strings |
| JS app with Laminar UI | `mathlify` (top-level) | Everything including DOM compilation + reactive rendering |
| Native CLI / embedded | `mathlify.native` | Parse, eval, fold, MathML strings |

### 5.2 Extension Points

| Extension | Mechanism |
|---|---|
| **Custom numeric types** | Provide a `given MathTrig[T]` to evaluate `MathExpr` over your type (e.g. interval arithmetic, symbolic types, dual numbers). |
| **Custom display** | Provide a `given MathShow[T]` for your type to control how numbers render in MathML. |
| **Custom rendering** | Use `MathMLSerializer.compile` to get a MathML string, then pipe it into your own rendering pipeline (React, htmx, server-side templating, etc.). |
| **AST manipulation** | The AST is a plain sealed trait hierarchy—pattern match and transform it freely (constant folding, symbolic differentiation, optimisation passes, etc.). |

### 5.3 What Mathlify Does NOT Do

- **Symbolic algebra**: No simplification rules, equation solving, or CAS features beyond constant folding.
- **Matrices as first-class evaluables**: `Matrix` and `MathVector` are AST nodes for display; `Evaluator.eval` on them returns an error. Matrix computation is expected to happen via vecxt or similar.
- **LaTeX parsing/output**: Input is AsciiMath or the simpler math syntax. Output is MathML, not LaTeX.
- **Implicit broadcasting / tensor ops**: By design (see vecxt philosophy).
- **Multi-statement / assignment**: The expression language is single-expression. No variable assignment, sequencing, or imperative constructs.
- **Error recovery in parsing**: Both parsers fail-fast. No partial parse results.

---

## 6. Typical Integration Workflow

```
1. Parse:    AsciiMath.translate("x^2 + sin(y)")           →  Right(MathExpr[Double])
2. Analyse:  Evaluator.freeVars(expr)                       →  Set("x", "y")
3. Evaluate: Evaluator.eval(expr, Map("x" → 3.0, "y" → 1.0)) →  Numeric(9.841...)
4. Render:   MathMLSerializer.compile(expr)                 →  "<math xmlns=...>...</math>"
   (or)      LaminarRenderer.render(expr)                   →  HtmlElement (browser)
```

Each step is independent. You can parse and render without evaluating, or evaluate without rendering.

---

## 7. Versioning & Publishing

- Version derived from VCS tags (`VcsVersion`).
- Published to Maven Central under `io.github.quafadas::mathlify`.
- Scala 3.8.x, Scala.js 1.20.x, Scala Native 0.5.x.