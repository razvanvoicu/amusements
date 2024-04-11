package sg.raz.www.js

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, Event, HTMLCanvasElement, HTMLDivElement, document, window}

type CRC2D = CanvasRenderingContext2D

def makeCanvas(container: HTMLDivElement): CRC2D =
  val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
  canvas.id = "maincanvas"
  val width = List(container.clientWidth, 3 * container.clientHeight / 5, 640).min
  val height = width
  canvas.style.width = "100%"
  canvas.style.height = "100%"
  canvas.style.left = "0px"
  canvas.style.top = "20px"
  canvas.style.position = "absolute"
  canvas.style.display = "block"
  canvas.style.margin = "0"
  canvas.width = width
  canvas.height = height
  container.appendChild(canvas)
  window.addEventListener("resize", (e: Event) =>
    val width = List(container.clientWidth, 3 * container.clientHeight / 5, 640).min
    val height = width
    canvas.style.width = s"${width}px"
    canvas.style.height = s"${height}px"
    canvas.style.left = s"${(container.clientWidth - width) / 2}px"
  )
  window.setTimeout(() =>
    canvas.width = List(container.clientWidth, 3 * container.clientHeight / 5, 640).min
    canvas.height = canvas.width
    canvas.style.width = s"${canvas.width}px"
    canvas.style.height = s"${canvas.height}px"
    canvas.style.left = s"${(container.clientWidth - canvas.width) / 2}px"
  , 10)
  canvas.getContext("2d").asInstanceOf[CRC2D]

type TColor = (Int, Int, Int)
def tColorToString(c: TColor) = s"rgb$c"

type Curve = Double => (Double, Double)
