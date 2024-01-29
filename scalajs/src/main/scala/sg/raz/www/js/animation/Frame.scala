package sg.raz.www.js.animation

import sg.raz.www.js.geometry.OpenOrClosedPath.ClosedPath
import sg.raz.www.js.geometry._
import sg.raz.www.js.geometry.morphCurves

import scala.collection.mutable
import scala.collection.mutable.ArrayDeque

class Plot(override val iterator: Iterator[Point], val n: Int) extends Iterable[Point]

class ColorPlot(plot: Plot, grad: ColorGradient) extends Iterable[ColorPoint]:
  override def iterator: Iterator[ColorPoint] =
    plot.iterator.zip(grad.makeIterator(plot.n)).map:
      case (p, c) => p.toColorPoint(c)

class ColorAndThicknessPlot(plot: ColorPlot, n: Int, startThickness: Int, endThickness: Int)
  extends Iterable[ColorPointWithThickness]:
  override def iterator: Iterator[ColorPointWithThickness] =
    val thickInc: Double = (endThickness - startThickness).toDouble / (n + 1)
    val thickIter: Iterator[Int] = Iterator.iterate(startThickness.toDouble, n+1)(t => t + thickInc).map(_.toInt)
    plot.iterator.zip(thickIter).map { case (p, t) => p.toColorPointWithThickness(t) }


class Frame(plot: ColorAndThicknessPlot, n: Int)(using center: CanvasCenter) extends Iterable[CanvasPoint]:
  override def iterator: Iterator[CanvasPoint] = plot.iterator.map(_.toCanvasPoint)

class StateFrame(resolution: Int, startThickness: Int, endThickness: Int, colorGradient: ColorGradient)(using center: CanvasCenter):
  private val currentPath: mutable.ArrayDeque[Point] = mutable.ArrayDeque.empty[Point]
  currentPath.ensureSize(resolution + 1)
  val futurePathPoints: mutable.ArrayDeque[Point] = mutable.ArrayDeque.empty[Point]
  futurePathPoints.ensureSize(resolution + 1)
  def next(action: Option[() => Unit] = None): Unit =
    if futurePathPoints.nonEmpty then
      currentPath.prepend(futurePathPoints.removeHead())
      if currentPath.length > resolution then currentPath.removeLast()
    else action.foreach(_())
  def addCurve(curve: Curve): Unit =
    futurePathPoints.addAll(Discretizer(resolution, ClosedPath) <<< curve)
    if currentPath.isEmpty then next()
  def lastPoint(): Option[Point] =
    currentPath.headOption.orElse(futurePathPoints.lastOption)
  def getFrame: Frame =
    if currentPath.isEmpty then next()
    Frame(
      ColorAndThicknessPlot(
        ColorPlot(
          Plot(currentPath.iterator, currentPath.length),
          colorGradient
        ),
        resolution, startThickness, endThickness
      ),
      resolution
    )
  def flush(): Unit =
    futurePathPoints.removeAll()
    currentPath.removeAll()

class MorphFrame(duration: Int, timeResolution: Int, resolution: Int, thickness: Int, colorGradient: ColorGradient)(using geoEnv: GeoEnv):
  private var curve1: Curve = unitCircle
  private var curve2: Option[Curve] = None

  private var state: Double = 0.0

  def next(): Unit =
    state += timeResolution.toDouble / duration
    if state > 1 then state = 1.0
    
  def setState(s: Double): Unit =
    assert ( 0.0 <= s && s <= 1.0)
    state = s

  private def morphingCurve: Curve = morphCurves(curve1, curve2.getOrElse(unitCircle), I01(state))

  def addCurve(curve: Curve): Unit =
    curve1 = curve2.getOrElse(unitCircle)
    curve2 = Option(curve)
    state = 0

  def getFrame(using ce: CanvasCenter): Frame = Frame (
    ColorAndThicknessPlot(
      ColorPlot(
        Discretizer(resolution, ClosedPath) <<< morphingCurve,
        colorGradient
      ),
      resolution, thickness, thickness
    ),
    resolution
  )


class ContourFrame(duration: Int, timeResolution: Int, resolution: Int, thickness: Int, colorGradient: ColorGradient)(using geoEnv: GeoEnv):
  private var curve1: Curve = unitCircle
  private var curve2: Option[Curve] = None

  private var state: Double = -0.1

  def next(): Unit =
    state += timeResolution.toDouble / duration
    if state > 1 then state = 1.0

  def setState(s: Double): Unit =
    assert(-0.1 <= s && s <= 1.0)
    state = s

  private def contourCurve: Curve = threadCurves(curve1, curve2.getOrElse(unitCircle), state)

  def addCurve(curve: Curve): Unit =
    curve1 = curve2.getOrElse(unitCircle)
    curve2 = Option(curve)
    state = 0

  def getFrame(using ce: CanvasCenter): Frame = Frame (
    ColorAndThicknessPlot(
      ColorPlot(
        Discretizer(resolution, ClosedPath) <<< contourCurve,
        colorGradient
        ),
      resolution, thickness, thickness
      ),
    resolution
    )
  

