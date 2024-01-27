package sg.raz.www.js

import org.scalajs.dom
import org.scalajs.dom.{Event, HTMLCanvasElement, HTMLDivElement, console, document, window}
import geometry.*
import sg.raz.www.js.animation.{Canvas, ContourFrame, MorphFrame, StateFrame}
import sg.raz.www.js.geometry.OpenOrClosedPath.{ClosedPath, OpenPath}

def shapes(e: dom.Event, container: HTMLDivElement): Unit =
  val ctx = makeCanvas(container)
  given cnvs: Canvas(ctx, 40)
  given GeoEnv = cnvs.geoEnv
  val currentCurve: CurveContainer = CurveContainer(unitCircle)
  given center: CanvasCenter = cnvs.center
  val sf = StateFrame(400, 15, 3, ColorGradient(RGB(0xff, 0xff, 0), RGB(0,0xff,0)))
  createScroller(container, currentCurve, sf, ToggableFlag(true))
  sf.addCurve(unitCircle)
  window.setInterval(() => window.requestAnimationFrame(_ => {
    cnvs.renderFrames(sf.getFrame)
    val cv = currentCurve.value
    sf.next(Option(() => sf.addCurve(cv)))
  }), 10)

def shapesDynamicStyling(elem: HTMLDivElement)(using ge: GeoEnv): Unit =
  elem.style.top = s"${ge.squareSize + 60}px"
  elem.style.height = s"${window.innerHeight - ge.squareSize - 105}px"
  val maxWidth = window.innerWidth - 10
  val width = (maxWidth / 115).toInt * 115
  elem.style.width = s"${width}px"
  elem.style.left = s"${(maxWidth - width) / 2}px"

def styleScroller(elem: HTMLDivElement)(using ge: GeoEnv): Unit =
  shapesDynamicStyling(elem)
  window.setTimeout(() => shapesDynamicStyling(elem), 100)
  window.addEventListener("resize", _ => shapesDynamicStyling(elem))

def makeButtonCanvas(curveName: String): HTMLCanvasElement =
  val cnvs = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
  cnvs.id = curveName
  cnvs.setAttribute("class", "shapes")
  cnvs.height = 400
  cnvs.width = 400
  cnvs

def buttonAction(
  currentCurve: CurveContainer, 
  curve: Curve, 
  frame: StateFrame|MorphFrame|ContourFrame, 
  a: ToggableFlag
): Event => Unit = _ =>
  a.value = true
  frame match {
    case stateFrame: StateFrame =>
      currentCurve.value = curve
      stateFrame.flush()
      val cv = currentCurve.value
      stateFrame.next(Option(() => stateFrame.addCurve(cv)))
    case morphFrame: MorphFrame =>
      currentCurve.value = curve
      morphFrame.addCurve(curve)
    case contourFrame: ContourFrame =>
      currentCurve.value = curve
      contourFrame.addCurve(curve)
  }

def renderCurveOnButton(cnvs: HTMLCanvasElement, curveName: String) =
  given cvsS: Canvas = Canvas(cnvs.getContext("2d").asInstanceOf[CRC2D], 5)
  given centerS: CanvasCenter = cvsS.center
  given GeoEnv = cvsS.geoEnv
  val cmS = curves.toMap
  val frame = StateFrame(400, 8, 8, ColorGradient(RGB(0xff, 0xb6, 0xc1), RGB(0xff, 0x69, 0xb4)))(using centerS)
  frame.addCurve(cmS(curveName))
  (0 to 400).foreach { _ => frame.next() }
  cvsS.renderFrames(frame.getFrame)


def createScroller(
  container: HTMLDivElement, 
  currentCurve: CurveContainer, 
  sf: StateFrame|MorphFrame|ContourFrame, 
  a: ToggableFlag
)(using ge: GeoEnv): HTMLDivElement =
  val elem = document.createElement("div").asInstanceOf[HTMLDivElement]
  elem.id = "scroller"
  styleScroller(elem)
  container.appendChild(elem)
  val cn = curves.map(_._1)
  val cm = curves.toMap
  cn.foreach { curveName =>
    val cnvs = makeButtonCanvas(curveName)
    cnvs.addEventListener("click", buttonAction(currentCurve, cm(curveName), sf, a))
    elem.appendChild(cnvs)
    renderCurveOnButton(cnvs, curveName)
  }
  elem

