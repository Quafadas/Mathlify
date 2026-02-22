# Copilot Instructions — Mathlify

## Project purpose

**Mathlify** is a Scala.js library that parses plain-text mathematical
expressions into a typed AST (`MathExpr`) and renders them as structured
Laminar HTML elements for display in a browser.

The primary entry-point for consumers:

```scala
import mathlify.*

val expr: MathExpr = mathlify"(a + b) + c = a + (b + c)"
val el:   HtmlElement = MathComponent.render(expr)
```

---

## Build system — Mill 1.1.0

| File | Purpose |
|------|---------|
| `.mill-version` | Pins the Mill version (currently `1.1.0`) |
| `build.mill` | Defines the `mathlify` Scala.js module and its `test` sub-module |
| `package.json` | npm devDependency `jsdom` (required by `JSDOMNodeJSEnv` in tests) |

### Common Mill commands

```bash
# Compile
mill mathlify.compile

# Run all tests (requires `npm install` first)
npm install
mill mathlify.test

# Run a single test suite
mill mathlify.test mathlify.MathParserSuite

# Format sources
mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources

# Produce a full-optimised JS bundle
mill mathlify.fullLinkJS
```

> Mill is downloaded automatically from GitHub Releases using the version in
> `.mill-version`.  Install it once with:
> ```bash
> curl -fsSL https://raw.githubusercontent.com/com-lihaoyi/mill/main/mill \
>   -o /usr/local/bin/mill && chmod +x /usr/local/bin/mill
> ```

---

## Source layout

```
mathlify/
  src/mathlify/
    MathExpr.scala       ← sealed enum — the AST
    MathParser.scala     ← tokeniser + recursive-descent parser
    MathComponent.scala  ← Laminar renderer (MathExpr → HtmlElement)
    package.scala        ← mathlify"…" string interpolator
  test/src/mathlify/
    MathParserSuite.scala    ← pure parser unit tests (no DOM)
    MathComponentSuite.scala ← Laminar DOM tests (requires jsdom)
```

---

## Key types

| Type | Location | Description |
|------|----------|-------------|
| `MathExpr` | `MathExpr.scala` | Sealed enum for the expression AST |
| `MathParseException` | `MathParser.scala` | Thrown when input cannot be parsed |
| `MathParser.parse` | `MathParser.scala` | `String → MathExpr` |
| `MathComponent.render` | `MathComponent.scala` | `MathExpr → HtmlElement` |
| `mathlify"…"` | `package.scala` | String interpolator shorthand |

---

## Adding a new operator

1. Add a new case to `MathExpr` (e.g. `case Pow(base: MathExpr, exp: MathExpr)`).
2. Extend the lexer/parser in `MathParser` to recognise the new token.
3. Add a render branch in `MathComponent.render`.
4. Add tests in `MathParserSuite` and `MathComponentSuite`.

---

## CSS class conventions

Every rendered `<span>` carries a semantic class usable for styling or DOM
queries in tests:

| Class | Element |
|-------|---------|
| `math-var` | Variable identifier |
| `math-num` | Numeric literal |
| `math-add` | Addition wrapper |
| `math-sub` | Subtraction wrapper |
| `math-mul` | Multiplication wrapper |
| `math-div` | Division wrapper |
| `math-equation` | Equality wrapper |
| `math-parens` | Parenthesised sub-expression |
| `math-op` | Any infix operator span |
| `math-plus`, `math-minus`, `math-times`, `math-div-sign`, `math-equals` | Specific operators |
| `math-paren-open`, `math-paren-close` | `(` and `)` spans |

---

## Testing style

Tests use **munit** and mirror two layers:

1. **`MathParserSuite`** — fast, pure Scala, no DOM.  Tests the parser in
   isolation using `assertEquals` on the `MathExpr` ADT.

2. **`MathComponentSuite`** — requires **jsdom** (enabled via `JSDOMNodeJSEnv`
   in `build.mill`).  Mounts a Laminar component with `render(container, el)`,
   queries the DOM with `querySelector`/`querySelectorAll`, asserts on
   structure, then kills the subscription and removes the container.

   Pattern:
   ```scala
   val (c, sub) = mount(MathExpr.Var("x"))
   assertEquals(c.querySelector(".math-var").textContent, "x")
   unmount(c, sub)
   ```

---

## CI

The GitHub Actions workflow at `.github/workflows/build.yml`:

1. Checks out the repo
2. Sets up JDK 21 and Node.js 20
3. Caches Mill + Ivy + Coursier artefacts
4. Downloads the pinned Mill binary
5. Runs `npm ci` (installs jsdom)
6. Compiles and runs tests

---

## Formatting

Code is formatted with **Scalafmt**.  Config is in `.scalafmt.conf`.
The dialect is set to `scala3` with optional-braces removal and new-syntax
rewrites enabled.

Run:
```bash
mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
```
