package sg.raz.www.js.geometry

import sg.raz.www.js.animation.Canvas

case class CanvasCenter(x: Int, y: Int):
  def asPoint(using ge: GeoEnv): Point = Point(x, y)
