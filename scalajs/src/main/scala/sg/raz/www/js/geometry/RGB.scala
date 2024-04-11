package sg.raz.www.js.geometry

import org.scalajs.dom.window

case class RGB(r: Int, g: Int, b: Int):
  private def convertToHex(k: Int) =
    val aux = "00" + Integer.toHexString(k % 256)
    aux.substring(aux.length - 2)
  def toCanvasString = s"#${convertToHex(r)}${convertToHex(g)}${convertToHex(b)}"
  def colorGradientTo(endColor: RGB, steps: Int): Array[RGB] =
    val result = Array.ofDim[RGB](steps)
    def cStep(rng: Int): Double = rng.toDouble / (steps - 1)
    val rRng = r - endColor.r
    val rStep = cStep(rRng)
    val gRng = g - endColor.g
    val gStep = cStep(gRng)
    val bRng = b - endColor.b
    val bStep = cStep(bRng)
    (0 to steps).foreach { i =>
      result(i) = RGB(
        r + (i * rStep).toInt,
        g + (i * gStep).toInt,
        b + (i * bStep).toInt
        )
    }
    result
  override def toString: String = toCanvasString

object RGB:
  def fromCanvasString(color: String): RGB =
    assert(color.charAt(0) == '#' && color.length == 7)
    RGB(
      Integer.parseInt(color.substring(1, 3), 16),
      Integer.parseInt(color.substring(3, 5), 16),
      Integer.parseInt(color.substring(5, 7), 16),
    )

class ColorGradient(start: RGB, end: RGB):
  def makeIterator(n: Int): Iterator[RGB] =
    val incR = (end.r - start.r).toDouble / (n+1)
    val incG = (end.g - start.g).toDouble / (n+1)
    val incB = (end.b - start.b).toDouble / (n+1)
    val aux = Iterator.iterate((start.r.toDouble, start.g.toDouble, start.b.toDouble), n):
      col => (col._1 + incR, col._2 + incG, col._3 + incB)
    aux.map :
      case (r, g, b) => RGB(Math.round(r).toInt, Math.round(g).toInt, Math.round(b).toInt)