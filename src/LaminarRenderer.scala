package mathlify

import com.raquo.laminar.api.L.*
import org.scalajs.dom

object LaminarRenderer:
  def render(expr: Signal[MathExpr]): HtmlElement =
    div(
      onMountCallback { ctx =>
        expr.foreach { e =>
          val el = ctx.thisNode.ref
          while (el.firstChild != null) el.removeChild(el.firstChild)
          el.appendChild(MathMLCompiler.toMathML(e))
        }(ctx.owner)
      }
    )
