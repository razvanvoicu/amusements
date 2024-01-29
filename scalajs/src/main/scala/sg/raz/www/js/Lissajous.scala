package sg.raz.www.js

import org.scalajs.dom
import org.scalajs.dom.window.setInterval
import org.scalajs.dom.{CanvasRenderingContext2D, Event, HTMLCanvasElement, HTMLDivElement, document, window}

import scala.collection.mutable

def lissajous(e: dom.Event, container: HTMLDivElement): Unit =
  val ctx = makeCanvas(container)
  val width = ctx.canvas.width
  val height = ctx.canvas.height
  ctx.lineWidth = 4.0
  ctx.strokeStyle = "#d6d640"
  val breadth = Math.min(width - 80, height - 150)
  val periodH = Period(1000)
  val periodV = Period(1500)
  val ho = HorizOsc(30, 70, breadth, periodH, 7, "cyan", 0).asInstanceOf[Osc]
  val vo = VertOsc(10, 100, breadth, periodV, 7, "magenta", 0).asInstanceOf[Osc]
  val circ = lissajousCircle(30 + breadth/2, 100 + breadth/2, breadth/2, "lightgreen", ho, vo)
  val ix = Intersect(ho, vo, 20, (0xd6,0xd6,0x40), (0, 240, 0))
  lissajousClock(ctx, width, height, 20, Array(ho, vo, circ, ix))
  makeControls(container, periodH, periodV, ho, vo, breadth)

def lissajousClock(ctx: CanvasRenderingContext2D, width: Int, height: Int, samplePeriod: Int, clients: Array[LissajousSprite]) =
  var time = 0L
  setInterval(() => window.requestAnimationFrame {_ => {
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height )
    clients.foreach(_.nextState(time))
    clients.foreach(_.draw(ctx, width, height))
    time += samplePeriod
  }}, samplePeriod)

case class Period(var period: Int)

trait LissajousSprite:
  def draw(ctx: CanvasRenderingContext2D, canvasWidth: Int, canvasHeight: Int): Unit
  def nextState(time: Long): Unit

case class lissajousCircle(val centerX: Int, val centerY: Int, val radius: Int, val color: String, ho: Osc, vo: Osc) extends LissajousSprite:
  override def nextState(time: Long): Unit = ()
  override def draw(ctx: CanvasRenderingContext2D, canvasWidth: Int, canvasHeight: Int): Unit =
    ctx.beginPath()
    ctx.strokeStyle = color
    ctx.fillStyle = color
    ctx.lineWidth = 2
    ctx.moveTo(centerX + radius, centerY)
    ctx.arc(centerX, centerY, radius, 0, 2 * Math.PI, false )
    ctx.stroke()
    val size = 25
    // val x1 = centerX + radius * Math.cos((ho.position - 90) * Math.PI / 180)
    val y1 = centerY + radius * Math.sin((ho.position) * Math.PI / 180)
    ctx.fillStyle = ho.color
    ctx.fillRect(ho.x - size/2, y1 - size/2, size, size )
    val x2 = centerX + radius * Math.cos(vo.position * Math.PI / 180)
    // val y2 = centerY + radius * Math.sin(vo.position * Math.PI / 180)
    ctx.fillStyle = vo.color
    ctx.fillRect(x2 - size / 2, vo.y - size / 2, size, size)

trait Osc extends LissajousSprite:
  def color: String
  var position: Double
  var x: Int = 0
  var y: Int = 0

case  class HorizOsc(
  left: Int,
  top: Int,
  width: Int,
  period: Period,
  size: Int,
  color: String,
  initialPosition: Int
) extends Osc:
  var position: Double = initialPosition // 0 to 359.999...
  private var oldTime: Long = 0
  override def nextState(time: Long): Unit =
    position += 360 * (time - oldTime).toDouble / period.period
    position = Option(position).filter(_ <= 360).getOrElse(position - 360)
    oldTime = time
  override def draw(ctx: CanvasRenderingContext2D, canvasWidth: Int, canvasHeight: Int): Unit =
    ctx.beginPath()
    ctx.strokeStyle = 0
    x = left + (width * (1 + Math.cos(Math.PI * position / 180)) / 2).toInt
    y = top
    ctx.strokeStyle = color
    ctx.fillStyle = color
    ctx.fillRect(x - size / 2, y - size / 2, size, size)
    ctx.fillRect(x, y, 2, width + 50)
    ctx.fillRect(x - size / 2, y + width + 50 - size / 2, size, size)
    ctx.stroke()

case  class VertOsc(
  val left: Int,
  val top: Int,
  val height: Int,
  val period: Period,
  val size: Int,
  val color: String,
  val initialPosition: Int
) extends Osc:
  var position: Double = initialPosition
  private var oldTime = 0L
  override def nextState(time: Long): Unit =
    position += (360 * (time - oldTime).toDouble / period.period)
    position = Option(position).filter(_ < 360).getOrElse(position - 360)
    oldTime = time
  override def draw(ctx: CanvasRenderingContext2D, canvasWidth: Int, canvasHeight: Int): Unit =
    ctx.beginPath()
    y = top + (height * (1 + Math.sin(Math.PI * position / 180)) / 2).toInt
    x = left
    ctx.strokeStyle = color
    ctx.fillStyle = color
    ctx.fillRect(x - size / 2, y - size / 2, size, size)
    ctx.fillRect(x, y, height + 50, 2)
    ctx.fillRect(x + height + 50 - size / 2, y - size / 2, size, size)
    ctx.stroke()

class Intersect(ox: Osc, oy: Osc, size: Int, val startColor: (Int, Int, Int), val endColor: (Int, Int, Int))
  extends LissajousSprite:
  private val trace: mutable.ArrayDeque[(Int, Int)] = mutable.ArrayDeque()
  trace.ensureSize(401)
  override def nextState(time: Long): Unit =  ()
  private def tracePoint(): Unit =
    trace.prepend((ox.x, oy.y))
    if trace.length > 400 then trace.removeLast()
  private def startPath(ctx: CanvasRenderingContext2D): Unit =
    ctx.beginPath()
    ctx.strokeStyle = tColorToString(startColor)
    ctx.lineWidth = size
    ctx.lineCap = "round"
    ctx.moveTo(ox.x, oy.y)

  private def drawAnotherSegment(ctx: CanvasRenderingContext2D, flag: Boolean, s: Int, idx: Int, x: Int, y: Int): Unit =
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

  override def draw(ctx: CanvasRenderingContext2D, canvasWidth: Int, canvasHeight: Int): Unit =
      tracePoint()
      startPath(ctx)
      val withSizes = trace.toSeq.zipWithIndex.map {
        case (coord, idx) => (coord, size * (trace.length + 1 - idx) / (trace.length + 1), idx)
      }
      withSizes.zip(withSizes.tail).foreach {
        case ((_, s, idx), ((x, y), t, _)) => drawAnotherSegment(ctx, s!=t, s, idx, x, y)
      }
      ctx.stroke()

def makeControls(container: HTMLDivElement, periodH: Period, periodV: Period, ho: Osc, vo: Osc, breadth: Int): Unit =
  val fH = document.createElement("div").asInstanceOf[HTMLDivElement]
  fH.style.display = "block"
  fH.style.position = "absolute"
  fH.style.color = "cyan"
  fH.style.fontSize = "40px"
  {
    val fht = document.createElement("div").asInstanceOf[HTMLDivElement]
    val fhm = document.createElement("div").asInstanceOf[HTMLDivElement]
    fhm.innerHTML = "-"
    fhm.style.display = "inline"
    fhm.addEventListener(
      "click", _ => {
        if periodH.period >= 200 then
          periodH.period -= 100
          fht.innerHTML = f"${periodH.period.toDouble / 1000}%4.1f"
      })
    fH.appendChild(fhm)
    fht.innerHTML = f"${periodH.period.toDouble / 1000}%4.1f"
    fht.style.display = "inline"
    fht.style.margin = "0 40px"
    fht.addEventListener(
      "click", _ => {
        ho.position -= 5
        if ho.position < 0 then ho.position = 360 + ho.position
      }
      )
    fH.appendChild(fht)
    val fhp = document.createElement("div").asInstanceOf[HTMLDivElement]
    fhp.innerHTML = "+"
    fhp.style.display = "inline"
    fhp.addEventListener(
      "click", _ => {
        periodH.period += 100
        fht.innerHTML = f"${periodH.period.toDouble / 1000}%4.1f"
      }
      )
    fH.appendChild(fhp)
  }
  container.appendChild(fH)
  fH.style.top = (breadth + 140) + "px"
  fH.style.left = "40px"

  val fV = document.createElement("div").asInstanceOf[HTMLDivElement]
  fV.style.display = "block"
  fV.style.position = "absolute"
  fV.style.color = "magenta"
  fV.style.fontSize = "40px"
  {
    val fht = document.createElement("div").asInstanceOf[HTMLDivElement]
    val fhm = document.createElement("div").asInstanceOf[HTMLDivElement]
    fhm.innerHTML = "-"
    fhm.style.display = "inline"
    fhm.addEventListener(
      "click", _ => {
        if periodV.period >= 200 then
          periodV.period -= 100
          fht.innerHTML = f"${periodV.period.toDouble / 1000}%4.1f"
      }
      )
    fV.appendChild(fhm)
    fht.innerHTML = f"${periodV.period.toDouble / 1000}%4.1f"
    fht.style.display = "inline"
    fht.addEventListener(
      "click", _ => {
        vo.position -= 5
        if vo.position < 0 then vo.position = 360 + vo.position
      }
      )
    fht.style.margin = "0 40px"
    fV.appendChild(fht)
    val fhp = document.createElement("div").asInstanceOf[HTMLDivElement]
    fhp.innerHTML = "+"
    fhp.style.display = "inline"
    fhp.addEventListener(
      "click", _ => {
        periodV.period += 100
        fht.innerHTML = f"${periodV.period.toDouble / 1000}%4.1f"
      }
      )
    fV.appendChild(fhp)
  }
  container.appendChild(fV)
  fV.style.top = (breadth + 210) + "px"
  fV.style.left = "40px"
  window.setInterval(
    () => {
      val h = fV.clientWidth
      fV.style.left = s"${(window.innerWidth - h) / 2}px"
      fH.style.left = fV.style.left
    }, 10
    )

