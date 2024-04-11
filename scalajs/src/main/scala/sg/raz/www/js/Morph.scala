package sg.raz.www.js

import org.scalajs.dom
import org.scalajs.dom.{HTMLDivElement, window}
import sg.raz.www.js
import sg.raz.www.js.animation.{Canvas, ContourFrame, MorphFrame}
import sg.raz.www.js.geometry.*

enum MorphMethod:
  case Morph extends MorphMethod
  case Contour extends MorphMethod

def morph(m: MorphMethod)(e: dom.Event, container: HTMLDivElement): Unit =
  val ctx = makeCanvas(container)
  given cnvs: Canvas(ctx, 40)
  given center: CanvasCenter = cnvs.center
  given GeoEnv = cnvs.geoEnv
  val currentCurve: CurveContainer = CurveContainer(unitCircle)
  import MorphMethod._
  val f : MorphFrame | ContourFrame = m match
    case Morph => MorphFrame(2000, 50, 5000, 10, ColorGradient(RGB(0xff, 0xff, 0), RGB(0, 0xff, 0)))
    case Contour => ContourFrame(6000, 25, 2000, 10, ColorGradient(RGB(0xff, 0xff, 0), RGB(0, 0xff, 0)))
  val animate = ToggableFlag(true)
  val scroller = createScroller(container, currentCurve, f, animate)
  window.setInterval(
    () => window.requestAnimationFrame(_ => {
      ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
      m match
        case MorphMethod.Morph =>
          val mf = f.asInstanceOf[MorphFrame]
          cnvs.renderFrames(mf.getFrame)
          if animate.value then mf.next()
        case MorphMethod.Contour =>
          val cf = f.asInstanceOf[ContourFrame]
          cnvs.renderFrames(cf.getFrame)
          if animate.value then cf.next()
    }), 20
  )
  def manualProgress(): Unit =
    if !animate.value then
      m match
        case Morph =>
          val mf = f.asInstanceOf[MorphFrame]
          mf.setState(scroller.scrollTop / (scroller.scrollHeight - scroller.style.height.stripSuffix("px").toInt))
        case Contour =>
          val cf = f.asInstanceOf[ContourFrame]
          cf.setState(
            (scroller.scrollTop / (scroller.scrollHeight - scroller.style.height.stripSuffix("px").toInt)) * 1.1 - 0.1
          )

  ctx.canvas.addEventListener("click", _ => {
    animate.value = ! animate.value
    manualProgress()
  })
  scroller.addEventListener("scroll", _ => manualProgress())

