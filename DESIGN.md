# Forward Mode Automatic Differentiation – Design, Implementation & Verification

## Overview

This document describes the design of a new **Forward Mode Automatic Differentiation** page
for the Mathlify example app, closing issue #45. The page teaches students how forward-mode AD
works through theory, worked examples, and an interactive evaluator.

The implementation uses **Spire** (`org.typelevel::spire::0.18.0`), specifically its `Jet[Double]`
dual number type, as specified in the issue title "(via spire)".

---

## 1. Design

### 1.1 Core Library: `ForwardDiff` module (via Spire)

Forward-mode AD works by augmenting every value with its derivative (a *dual number*).
Spire's `Jet[Double]` provides exactly this: `Jet(real, infinitesimal)` where `real` is
the function value and `infinitesimal` is an array of partial derivatives.

We use `JetDim(n)` where `n` is the number of free variables, assigning one infinitesimal
dimension per variable. This means **all partial derivatives are computed simultaneously in
a single evaluator pass** — the i-th component of the result's infinitesimal array holds ∂f/∂xᵢ.

We bridge Spire's algebra to mathlify's evaluator by dynamically creating a `MathTrig[Jet[Double]]`
instance for the required dimension, delegating trig/exp/log/sqrt to Spire's type class instances.

**Dual number arithmetic:**

| Operation | Value | Derivative |
|-----------|-------|------------|
| `(a,a') + (b,b')` | `a + b` | `a' + b'` |
| `(a,a') - (b,b')` | `a - b` | `a' - b'` |
| `(a,a') * (b,b')` | `a * b` | `a'*b + a*b'` |
| `(a,a') / (b,b')` | `a / b` | `(a'*b - a*b') / b²` |
| `(a,a') ^ n`       | `aⁿ`   | `n * a^(n-1) * a'` |
| `sin(a,a')`        | `sin(a)` | `cos(a) * a'` |
| `cos(a,a')`        | `cos(a)` | `-sin(a) * a'` |
| `exp(a,a')`        | `exp(a)` | `exp(a) * a'` |
| `log(a,a')`        | `log(a)` | `a' / a` |
| `sqrt(a,a')`       | `√a`    | `a' / (2√a)` |
| `tan(a,a')`        | `tan(a)` | `a' / cos²(a)` |

**Implementation approach:**

- Define a `Dual` case class holding `(value: Double, deriv: Double)`.
- Provide `given MathTrig[Dual]` so the existing generic `Evaluator.eval[A]` works
  with dual numbers out of the box — no new evaluation code needed.
- To compute ∂f/∂xᵢ, seed `xᵢ` with `Dual(xᵢ, 1.0)` and all other variables with
  `Dual(xⱼ, 0.0)`. The resulting `.deriv` field is the partial derivative.

This is elegant because it reuses the entire existing evaluator infrastructure.

### 1.2 Example Page: `AutoDiffPage.scala`

The page is divided into three sections:

#### Section A — Theory

Three subsections displayed as informational cards:

1. **What is Forward-Mode AD?**
   Brief introduction: dual numbers, how derivatives propagate.

2. **The Chain Rule**
   Statement of the chain rule with a worked example.

3. **The Quotient Rule**
   Statement of the quotient rule with a worked example.

Each theory section renders AsciiMath expressions using `LaminarRenderer.render`.

#### Section B — Worked Examples

Interactive cards that show step-by-step derivations from first principles:

1. **f(x) = x²** — derivative is 2x
2. **f(x) = eˣ** — derivative is eˣ
3. **f(x) = e^(x²)** — derivative is 2x·e^(x²) (chain rule example)
4. **Partial derivatives: f(x,y) = x²y + y³** — ∂f/∂x = 2xy, ∂f/∂y = x² + 3y²

Each example shows:
- The function definition (MathML rendered)
- The analytical derivative (MathML rendered)
- A numeric evaluation at a sample point using the ForwardDiff evaluator
- Verification that AD matches the analytical result

#### Section C — Interactive Evaluator

Similar to the existing ExpressionPage but enhanced:

1. **Expression Input** — AsciiMath textarea (same as ExpressionPage)
2. **Variable Values** — Input fields for each free variable (same pattern)
3. **Differentiation Variable** — Dropdown to select which variable to differentiate with respect to
4. **Results** — Shows both:
   - The function value f(x₁,...,xₙ)
   - The partial derivative ∂f/∂xᵢ at the given point

### 1.3 Routing & Navigation

- Add `case object AutoDiff extends Page` to the `Page` sealed trait
- Add route: `Route.static(Page.AutoDiff, root / "autodiff", basePath = appBasePath)`
- Add to router's serialize/deserialize/getPageTitle
- Add to SplitRender in `app`
- Add card on HomePage

---

## 2. Implementation Plan

### Step 1: `ForwardDiff.scala` (mathlify/src/)

Uses Spire's `Jet[Double]` with `JetDim(n)` where `n` = number of free variables:

```
import spire.math.{Jet, JetDim}

object ForwardDiff:
  case class DiffResult(value: Double, partials: Map[String, Double])

  private def makeMathTrig(n: Int): MathTrig[Jet[Double]]  // dimension-aware factory

  def gradient(expr, env): Either[String, DiffResult]       // all partials in one pass
  def differentiate(expr, env, wrt): Either[String, (Double, Double)]  // convenience
```

Each variable `xᵢ` is seeded with `Jet(v, inf)` where `inf(i) = 1.0` and all other
components are 0. The evaluator computes with `n`-dimensional Jets, and the result Jet's
`infinitesimal(i)` holds ∂f/∂xᵢ.

### Step 2: `ForwardDiffSpec.scala` (mathlify/test/src/)

Unit tests covering:
- Dual arithmetic (addition, subtraction, multiplication, division, power)
- Transcendental functions (sin, cos, exp, log, sqrt, tan)
- Chain rule compositions (e^(x²), sin(x²))
- Partial derivatives (multi-variable)
- Edge cases (division by zero, constants)

### Step 3: `AutoDiffPage.scala` (example/src/)

Implements the three-section page described above using Laminar reactive patterns,
following the same Card/Callout/Input component conventions as ExpressionPage.

### Step 4: Route integration (example/src/main.scala)

Add the new page to routing, serialization, and the page splitter.

### Step 5: HomePage card (example/src/HomePage.scala)

Add a card with description and navigation button.

### Step 6: CSS (example/assets/custom.css)

Add styles for:
- `.theory-section`, `.example-section`, `.evaluator-section`
- `.worked-example` cards
- `.derivative-result` styling
- `.diff-variable-select` dropdown

### Step 7: Playwright tests (appTest/src/autodiffPage.test.scala)

E2E tests:
- Page loads with theory, examples, and evaluator sections
- Worked examples display correct values
- Evaluator computes function value and derivative
- Changing differentiation variable updates the derivative
- Multi-variable expressions show correct partial derivatives

---

## 3. Verification

### 3.1 Unit Tests

The `ForwardDiffSpec` will verify correctness against known analytical derivatives:

| Expression | Variable | Point | Expected f | Expected f' |
|-----------|----------|-------|-----------|-------------|
| x² | x | x=3 | 9 | 6 |
| eˣ | x | x=0 | 1 | 1 |
| e^(x²) | x | x=1 | e | 2e |
| x²y + y³ | x | x=2,y=3 | 39 | 12 |
| x²y + y³ | y | x=2,y=3 | 39 | 31 |
| sin(x) | x | x=0 | 0 | 1 |
| x/y | x | x=4,y=2 | 2 | 0.5 |
| x/y | y | x=4,y=2 | 2 | -1 |

### 3.2 App Tests

Playwright tests will verify:
- Navigation from home page works
- All three sections are rendered
- Worked example cards show mathematical content
- Evaluator correctly displays value and derivative
- Changing the differentiation variable updates results

### 3.3 Existing Tests

All existing tests must continue to pass — the Spire-backed `MathTrig[Jet[Double]]` type class
instance and page additions are purely additive and don't modify any existing code paths.

---

## 4. Design Decisions & Rationale

1. **Use Spire's `Jet[Double]`**: As specified in the issue title "(via spire)", we use
   Spire's battle-tested `Jet` type for dual number arithmetic. A `MathTrig[Jet[Double]]`
   bridges Spire's algebra to the existing mathlify evaluator, delegating `sin`, `cos`, `exp`,
   `log`, `sqrt` etc. to Spire's type class instances.

2. **Reuse generic evaluator**: By implementing `MathTrig[Jet[Double]]`, we get automatic
   differentiation "for free" through the existing `Evaluator.eval[A]`. This is
   both elegant and minimizes new code.

3. **Forward mode (not reverse)**: Forward mode is simpler to explain and implement.
   It naturally computes one directional derivative per pass, which maps well to
   the educational goal of showing how derivatives propagate through computation.

4. **Spire in shared code**: `ForwardDiff.scala` lives in `mathlify/src/`
   (shared JVM/JS/Native) so the same differentiation logic works everywhere.
   Spire is added as a dependency to all platform modules.

5. **Page structure mirrors ExpressionPage**: Students familiar with the expression
   evaluator will find the AD evaluator intuitive since it follows the same pattern.
