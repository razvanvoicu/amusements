package sg.raz.www.js.geometry

import sg.raz.www.js.animation.Canvas

class CanvasPoint(val x: Int, val y: Int, val color: String, val thickness: Int) {

}

object CanvasPoint:
  def apply(x: Int, y: Int, color: String, thickness: Int) =
    new CanvasPoint(x, y, color, thickness)
  def fromPoint(p: ColorPointWithThickness)(using cvs: Canvas) =
    implicit val center = cvs.center
    p.toCanvasPoint
