package sg.raz.www.js.animation

import sg.raz.www.js.geometry.RGB

class State[T](var value: T, var updateUI: Option[T => Unit] = None):
  def fromString(s: String) = value match
    case _: Int => value = s.toInt.asInstanceOf[T]
    case _: Double => value = s.toDouble.asInstanceOf[T]
    case _: RGB => value = RGB.fromCanvasString(s).asInstanceOf[T]
