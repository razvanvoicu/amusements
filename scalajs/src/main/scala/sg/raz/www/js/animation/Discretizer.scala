package sg.raz.www.js.animation

import sg.raz.www.js.animation.Plot
import sg.raz.www.js.geometry.OpenOrClosedPath.{ClosedPath, OpenPath}
import sg.raz.www.js.geometry.{Curve, OpenOrClosedPath}
import sg.raz.www.js.geometry.{discretize, firstPoint}

class Discretizer(val n: Int, val flag: OpenOrClosedPath):
  def <<<(curve: Curve): Plot =
    val aux = curve.discretize(n).iterator
    flag match {
      case OpenPath => Plot(aux, n)
      case ClosedPath => Plot(aux ++ Iterator(curve.firstPoint), n + 1)
    }
