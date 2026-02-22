# Mathlify

A Scala.js library that parses plain-text mathematical expressions into a
typed AST and renders them as structured Laminar HTML elements.

```scala
import mathlify.*

val expr: MathExpr    = mathlify"(a + b) + c = a + (b + c)"
val el:   HtmlElement = MathComponent.render(expr)
render(dom.document.querySelector("#app"), el)
```

## Quick start

```bash
# Install jsdom (needed for tests)
npm install

# Compile
mill mathlify.compile

# Test
mill mathlify.test
```

## Build

- **Build system**: [Mill](https://mill-build.com) `1.1.0`  
- **Language**: Scala.js (Scala `3.6.2`, ScalaJS `1.17.0`)  
- **UI**: [Laminar](https://laminar.dev) `17.0.0`  
- **Testing**: [munit](https://scalameta.org/munit/) `1.0.3` with jsdom

See [`.github/copilot-instructions.md`](.github/copilot-instructions.md) for
full developer documentation.
