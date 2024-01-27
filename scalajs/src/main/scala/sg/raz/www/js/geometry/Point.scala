package sg.raz.www.js.geometry

import org.scalajs.dom.console

sealed class Point(val x: Int, val y: Int)(using ge: GeoEnv):
//  assert(canvas.minX - canvas.drawingMargin <= x && x <= canvas.maxX + canvas.drawingMargin &&
//    canvas.minY - canvas.drawingMargin <= y && y <= canvas.maxY + canvas.drawingMargin,
//    s"x = $x, y = $y"
//  )
  def toColorPoint(c: RGB): ColorPoint = ColorPoint(x, y, c)
  def radius: Double = Math.sqrt(x*x + y*y)
  def rotate(radians: Double): Point = 
    Point(
      (x * Math.cos(radians) - y*Math.sin(radians)).toInt,
      (x*Math.sin(radians) + y*Math.cos(radians)).toInt
      )
  def scale(factor: Double): Point = Point((x * factor).toInt, (y * factor).toInt)
  def translate(distX: Int, distY: Int): Point = Point(x + distX, y + distY)
  def mirrorX: Point = Point(-x, y)
  def mirrorY: Point = Point(x, -y)
  def mirror: Point = Point(-x, -y)
  def segmentTo(p: Point) = (t: Double) => Point((x * (1-t) + p.x * t).toInt, (y * (1-t) + p.y * t).toInt)

class ColorPoint(x: Int, y: Int, color: RGB)(using ge: GeoEnv) extends Point(x, y):
  def toColorPointWithThickness(thickness: Int): ColorPointWithThickness =
    ColorPointWithThickness(x, y, color, thickness)

class ColorPointWithThickness(x: Int, y: Int, color: RGB, thickness: Int)(using ge: GeoEnv)
  extends ColorPoint(x, y, color):
  given CanvasCenter = ge.center
  def toCanvasPoint(using center: CanvasCenter): CanvasPoint =
    CanvasPoint(x + center.x, center.y - y, color.toCanvasString, thickness)