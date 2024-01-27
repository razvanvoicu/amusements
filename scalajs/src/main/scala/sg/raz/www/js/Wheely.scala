package sg.raz.www.js

import org.scalajs.dom.{CanvasRenderingContext2D, Event, HTMLDivElement, window}

import scala.math.Numeric
import scala.math.Numeric.given
import scala.math.given

def wheely(e: Event, container: HTMLDivElement): Unit =
  val ctx = makeCanvas(container)
  val width = ctx.canvas.width
  val height = ctx.canvas.height
  val breadth = Math.min(width, height)
  val samplePeriod = 20
  val clock = Clock(ctx, samplePeriod)
  val period = 2000
  val angle = new ValueSprite[Int]:
    override def nextState(time: Long): Unit =
      _v = ((time % period) * 360 / period).toInt
  val hBar = new StatelessSprite:
    override def render(ctx: CRC2D): Unit =
      ctx.beginPath()
      ctx.lineWidth = 3
      ctx.strokeStyle = "#39ff14"
      ctx.moveTo(0, breadth / 2)
      ctx.lineTo(breadth, breadth / 2)
      ctx.stroke()
  val vBar = new StatelessSprite:
    override def render(ctx: CRC2D): Unit =
      ctx.beginPath()
      ctx.lineWidth = 3
      ctx.strokeStyle = "#39ff14"
      ctx.moveTo(breadth / 2, 0)
      ctx.lineTo(breadth / 2, breadth)
      ctx.stroke()
  val y0 = new ValueSprite[Int]:
    override def nextState(time: Long): Unit =
      _v = ((1 + Math.sin(Math.PI * angle.value / 180)) * breadth / 2).toInt
  val x1 = new ValueSprite[Int]:
    override def nextState(time: Long): Unit =
      _v = ((1 + Math.cos(Math.PI * angle.value / 180)) * breadth / 2).toInt
  val circDot = new StatelessSprite:
    override def render(ctx: CRC2D): Unit =
      ctx.beginPath()
      ctx.fillStyle = "red"
      ctx.strokeStyle = "red"
      ctx.lineWidth = 1
      ctx.ellipse((x1.value + breadth/2) / 2, (y0.value + breadth/2) / 2, 15, 15, 0, 0, 2 * Math.PI)
      ctx.fill()
      ctx.stroke()
  val rod = new StatelessSprite:
    override def render(ctx: CRC2D): Unit =
      ctx.beginPath()
      ctx.lineWidth = 11
      ctx.lineCap = "round"
      ctx.strokeStyle = "yellow"
      ctx.moveTo(breadth/2, y0.value)
      ctx.lineTo(x1.value, breadth/2)
      ctx.stroke()
  val dot = new StatelessSprite:
    override def render(ctx: CRC2D): Unit =
      ctx.beginPath()
      ctx.fillStyle = "cyan"
      ctx.strokeStyle = "cyan"
      ctx.lineWidth = 1
      ctx.moveTo(x1.value, breadth / 2)
      ctx.ellipse(x1.value, breadth / 2, 15, 15, 0, 0, 2 * Math.PI)
      ctx.closePath()
      ctx.stroke()
      ctx.fill()
      ctx.moveTo(breadth / 2, y0.value)
      ctx.beginPath()
      ctx.ellipse(breadth / 2, y0.value, 15, 15, 0, 0, 2 * Math.PI)
      ctx.fill()
      ctx.stroke()
  val marks = new StatelessSprite:
    override def render(ctx: CRC2D): Unit =
      val ratio1 = 0.2d
      val ratio2 = 0.8d
      ctx.beginPath()
      ctx.fillStyle = "#f0f000"
      ctx.strokeStyle = "#f0f000"
      ctx.lineWidth = 1
      ctx.moveTo(ratio1 * x1.value + (1-ratio1)*breadth/2, ratio1 * breadth / 2 + (1-ratio1)*y0.value)
      ctx.ellipse(ratio1 * x1.value + (1-ratio1)*breadth/2, ratio1 * breadth / 2 + (1-ratio1)*y0.value, 10, 10, 0, 0, 2 * Math.PI)
      ctx.closePath()
      ctx.stroke()
      ctx.fill()
      ctx.moveTo(ratio2 * x1.value + (1-ratio2)*breadth/2, ratio2 * breadth / 2 + (1-ratio2)*y0.value)
      ctx.beginPath()
      ctx.ellipse(ratio2 * x1.value + (1-ratio2)*breadth/2, ratio2 * breadth / 2 + (1-ratio2)*y0.value, 10, 10, 0, 0, 2 * Math.PI)
      ctx.fill()
      ctx.stroke()

  clock(
    Wheel(breadth / 2, breadth / 2, breadth / 4, 2, (0xec, 0x44, 0x9b)),
    hBar, vBar,
    angle,
    y0, x1,
    circDot,
    dot,
    Trail(x1, y0, breadth, 0.2, 10, (0xf0,0xc0,0xc0), (0x61,0x46,0x25)),
    Trail(x1, y0, breadth, 0.8, 10, (0xf0,0xc0,0xc0), (0x61,0x46,0x25)),
    rod,
    marks,
    circDot,
    dot,
    )


trait Sprite:
  def nextState(clock: Long): Unit
  def render(ctx: CRC2D): Unit

trait StatelessSprite extends Sprite:
  override def nextState(clock: Long): Unit = {}

abstract class ValueSprite[T : Numeric] extends Sprite:
  protected var _v: T = implicitly[Numeric[T]].zero
  def value: T = _v
  override def render(ctx: CRC2D): Unit = {}

case class Clock(val ctx: CanvasRenderingContext2D, val samplePeriod: Int):
  private var _clock: Long = 0L
  def clock: Long = _clock
  def apply(sprites: Sprite*): Unit = window.setInterval(() => window.requestAnimationFrame { t =>
    sprites.foreach(_.nextState(_clock))
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    sprites.foreach(_.render(ctx))
    _clock += samplePeriod
  }, samplePeriod)

case class Wheel(centerX: Int, centerY: Int, radius: Int, strokeWidth: Int, color: TColor)
  extends StatelessSprite:
  override def render(ctx: CRC2D): Unit =
    ctx.beginPath()
    ctx.strokeStyle = tColorToString(color)
    ctx.lineWidth = strokeWidth
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI)
    ctx.stroke()

case class Trail(
  x: ValueSprite[Int],
  y: ValueSprite[Int],
  breadth: Int,
  ratio: Double,
  strokeWidth: Int,
  startColor: TColor,
  endColor: TColor
) extends StatelessSprite:
  private val trace = scala.collection.mutable.ArrayDeque[(Int, Int)]()
  trace.ensureSize(100)
  override def render(ctx: CRC2D): Unit =
    val xnow = (breadth / 2 * ratio + x.value * (1 - ratio)).toInt
    val ynow = (y.value * ratio + breadth / 2 * (1 - ratio)).toInt
    trace.prepend((xnow, ynow))
    if trace.length > 100 then trace.removeLast()

    def startPath(ctx: CanvasRenderingContext2D): Unit =
      ctx.beginPath()
      ctx.strokeStyle = tColorToString(startColor)
      ctx.lineWidth = strokeWidth
      ctx.lineCap = "round"
      ctx.moveTo(xnow, ynow)

    def drawAnotherSegment(ctx: CanvasRenderingContext2D, flag: Boolean, s: Int, idx: Int, x: Int, y: Int): Unit =
      def sel(f: ((Int, Int, Int)) => Int) = (f(startColor) * (trace.length - idx) + f(endColor) * idx) / trace.length

      def color = s"rgb(${sel(_._1)},${sel(_._2)},${sel(_._3)})"

      ctx.lineWidth = s
      ctx.lineTo(x, y)
      if flag then
        ctx.strokeStyle = color
        ctx.fillStyle = color
        ctx.stroke()
        ctx.beginPath()
        ctx.moveTo(x, y)

    startPath(ctx)
    val withSizes = trace.toSeq.zipWithIndex.map {
      case (coord, idx) => (coord, strokeWidth * (trace.length + 1 - idx) / (trace.length + 1), idx)
    }
    withSizes.zip(withSizes.tail).foreach {
      case ((_, s, idx), ((x, y), t, _)) => drawAnotherSegment(ctx, s != t, s, idx, x, y)
    }
    ctx.stroke()
