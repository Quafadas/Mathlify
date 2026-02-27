package mathlify

import com.raquo.laminar.api.L.*

object LaminarRenderer:
  def render(expr: Signal[MathExpr]): HtmlElement =
    div(
      onMountCallback { ctx =>
        expr.foreach { e =>
          val el = ctx.thisNode.ref
          while el.firstChild != null do el.removeChild(el.firstChild)
          end while
          el.appendChild(MathMLCompiler.toMathML(e))
        }(using ctx.owner)
      }
    )

  def render(expr: MathExpr): HtmlElement =
    div(
      onMountCallback { ctx =>
        val el = ctx.thisNode.ref
        while el.firstChild != null do el.removeChild(el.firstChild)
        end while
        el.appendChild(MathMLCompiler.toMathML(expr))
      }
    )
end LaminarRenderer
