package sg.raz.www.js.animation

import sg.raz.www.js.animation.Plot
import sg.raz.www.js.geometry.{Curve, discretize}

class Plotter(nPoints: Int):
  def <<<(curve: Curve): Plot = Plot(curve.discretize(nPoints).iterator, nPoints)