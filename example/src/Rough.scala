package roughjs

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import org.scalajs.dom

/** Options controlling how rough shapes are drawn. All fields are optional – leave as `js.undefined` to use the library defaults.
  */
trait RoughOptions extends js.Object:
  var roughness: js.UndefOr[Double] = js.undefined
  var bowing: js.UndefOr[Double] = js.undefined
  var stroke: js.UndefOr[String] = js.undefined
  var strokeWidth: js.UndefOr[Double] = js.undefined
  var fill: js.UndefOr[String] = js.undefined

  /** "hachure" | "solid" | "zigzag" | "cross-hatch" | "dots" | "dashed" | "zigzag-line" */
  var fillStyle: js.UndefOr[String] = js.undefined
  var fillWeight: js.UndefOr[Double] = js.undefined
  var hachureAngle: js.UndefOr[Double] = js.undefined
  var hachureGap: js.UndefOr[Double] = js.undefined
  var seed: js.UndefOr[Int] = js.undefined
  var simplification: js.UndefOr[Double] = js.undefined
  var preserveVertices: js.UndefOr[Boolean] = js.undefined
end RoughOptions

/** Facade for a RoughCanvas instance (draws on a <canvas> element). */
@js.native
trait RoughCanvas extends js.Object:
  def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit = js.native
  def line(x1: Double, y1: Double, x2: Double, y2: Double, options: RoughOptions): Unit = js.native

  def rectangle(x: Double, y: Double, width: Double, height: Double): Unit = js.native
  def rectangle(x: Double, y: Double, width: Double, height: Double, options: RoughOptions): Unit = js.native

  def ellipse(x: Double, y: Double, width: Double, height: Double): Unit = js.native
  def ellipse(x: Double, y: Double, width: Double, height: Double, options: RoughOptions): Unit = js.native

  def circle(x: Double, y: Double, diameter: Double): Unit = js.native
  def circle(x: Double, y: Double, diameter: Double, options: RoughOptions): Unit = js.native

  def linearPath(points: js.Array[js.Array[Double]]): Unit = js.native
  def linearPath(points: js.Array[js.Array[Double]], options: RoughOptions): Unit = js.native

  def polygon(vertices: js.Array[js.Array[Double]]): Unit = js.native
  def polygon(vertices: js.Array[js.Array[Double]], options: RoughOptions): Unit = js.native

  def arc(x: Double, y: Double, width: Double, height: Double, start: Double, stop: Double, closed: Boolean): Unit = js.native
  def arc(x: Double, y: Double, width: Double, height: Double, start: Double, stop: Double, closed: Boolean, options: RoughOptions): Unit = js.native

  def curve(points: js.Array[js.Array[Double]]): Unit = js.native
  def curve(points: js.Array[js.Array[Double]], options: RoughOptions): Unit = js.native

  def path(d: String): Unit = js.native
  def path(d: String, options: RoughOptions): Unit = js.native
end RoughCanvas

/** Facade for a RoughSVG instance (returns SVG <g> elements to insert into the DOM). */
@js.native
trait RoughSVG extends js.Object:
  def line(x1: Double, y1: Double, x2: Double, y2: Double): dom.Element = js.native
  def line(x1: Double, y1: Double, x2: Double, y2: Double, options: RoughOptions): dom.Element = js.native

  def rectangle(x: Double, y: Double, width: Double, height: Double): dom.Element = js.native
  def rectangle(x: Double, y: Double, width: Double, height: Double, options: RoughOptions): dom.Element = js.native

  def ellipse(x: Double, y: Double, width: Double, height: Double): dom.Element = js.native
  def ellipse(x: Double, y: Double, width: Double, height: Double, options: RoughOptions): dom.Element = js.native

  def circle(x: Double, y: Double, diameter: Double): dom.Element = js.native
  def circle(x: Double, y: Double, diameter: Double, options: RoughOptions): dom.Element = js.native

  def linearPath(points: js.Array[js.Array[Double]]): dom.Element = js.native
  def linearPath(points: js.Array[js.Array[Double]], options: RoughOptions): dom.Element = js.native

  def polygon(vertices: js.Array[js.Array[Double]]): dom.Element = js.native
  def polygon(vertices: js.Array[js.Array[Double]], options: RoughOptions): dom.Element = js.native

  def arc(x: Double, y: Double, width: Double, height: Double, start: Double, stop: Double, closed: Boolean): dom.Element = js.native
  def arc(x: Double, y: Double, width: Double, height: Double, start: Double, stop: Double, closed: Boolean, options: RoughOptions): dom.Element = js.native

  def curve(points: js.Array[js.Array[Double]]): dom.Element = js.native
  def curve(points: js.Array[js.Array[Double]], options: RoughOptions): dom.Element = js.native

  def path(d: String): dom.Element = js.native
  def path(d: String, options: RoughOptions): dom.Element = js.native
end RoughSVG

/** Entry-point object – mirrors the default export of the `roughjs` npm package. */
@js.native
@JSImport("roughjs", JSImport.Default)
object Rough extends js.Object:
  def canvas(el: dom.HTMLCanvasElement): RoughCanvas = js.native
  def canvas(el: dom.HTMLCanvasElement, config: js.Object): RoughCanvas = js.native

  def svg(el: dom.SVGSVGElement): RoughSVG = js.native
  def svg(el: dom.SVGSVGElement, config: js.Object): RoughSVG = js.native
end Rough
