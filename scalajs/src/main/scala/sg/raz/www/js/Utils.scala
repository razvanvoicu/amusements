package sg.raz.www.js

import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, Event, HTMLCanvasElement, HTMLDivElement, document, window}

type CRC2D = CanvasRenderingContext2D

def makeCanvas(container: HTMLDivElement): CRC2D =
  val canvas = document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
  val width = container.clientWidth
  val height = container.clientHeight
  canvas.style.width = "100%"
  canvas.style.height = "100%"
  canvas.style.left = "0px"
  canvas.style.top = "0px"
  canvas.style.position = "absolute"
  canvas.style.display = "block"
  canvas.style.margin = "0"
  canvas.width = width
  canvas.height = height
  container.appendChild(canvas)
  window.addEventListener("resize", (e: Event) =>
    canvas.width = container.clientWidth
    canvas.height = container.clientHeight
  )
  window.setTimeout(() =>
    canvas.width = container.clientWidth
    canvas.height = container.clientHeight
  , 10)
  canvas.getContext("2d").asInstanceOf[CRC2D]

type TColor = (Int, Int, Int)
def tColorToString(c: TColor) = s"rgb$c"

type Curve = Double => (Double, Double)
