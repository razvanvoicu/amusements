package sg.raz.www.js.animation

import org.scalajs.dom.{CanvasRenderingContext2D, console, window}
import sg.raz.www.js.animation.Frame
import sg.raz.www.js.geometry.{CanvasCenter, CanvasPoint, GeoEnv}

case class Canvas(ctx: CanvasRenderingContext2D, drawingMargin: Int):
  def diameter = Math.min(ctx.canvas.width, ctx.canvas.height)
  def center: CanvasCenter = CanvasCenter(diameter / 2, diameter / 2)
  def maxX: Int = ctx.canvas.width / 2 - drawingMargin
  def maxY: Int = ctx.canvas.height / 2 - drawingMargin
  def minX: Int = - maxX
  def minY: Int = - maxY
  def width: Int = ctx.canvas.width - 2 * drawingMargin
  def height: Int = ctx.canvas.height - 2 * drawingMargin
  def squareSize: Int = Math.min(width, height)

  def geoEnv: GeoEnv = GeoEnv(
    diameter: Int, maxX: Int, maxY: Int, minX: Int, minY: Int, width: Int, height: Int, squareSize: Int,
    center: CanvasCenter,
  )
  def renderFrames(frames: Frame*): Unit = frames.foreach { frame =>
    val seq = frame.to(List)
    assert(seq.headOption.nonEmpty)
    val first = seq.head
    val pointSeq = seq.tail.zip(seq).zipWithIndex
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    ctx.beginPath()
    setDrawingStyle(first)
    ctx.moveTo(first.x, first.y)
    pointSeq.foreach:
      case ((cpCur, cpPrev), idx) =>
        changeColorAndThicknessIfNeeded(cpCur, cpPrev)
        ctx.lineTo(cpCur.x, cpCur.y)
    ctx.stroke()
  }

  private def setDrawingStyle(firstCanvasPoint: CanvasPoint) =
    ctx.lineCap = "round"
    ctx.lineJoin = "round"
    ctx.strokeStyle = firstCanvasPoint.color
    ctx.lineWidth = firstCanvasPoint.thickness

  private def changeColorAndThicknessIfNeeded(cpCur: CanvasPoint, cpPrev: CanvasPoint) =
    if cpCur.thickness != cpPrev.thickness || cpCur.color != cpPrev.color then
      ctx.stroke()
      ctx.beginPath()
      ctx.moveTo(cpPrev.x, cpPrev.y)
      ctx.lineWidth = cpCur.thickness
      ctx.strokeStyle = cpCur.color
