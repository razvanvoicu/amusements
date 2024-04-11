package sg.raz.www.js

import org.scalajs.dom.{HTMLCanvasElement, Event, HTMLAnchorElement, HTMLDivElement, KeyboardEvent, document, window}
import animation.*
import geometry.*
import org.scalajs.dom.html.Input

import scala.math.{Numeric, atan2}
import scala.math.Numeric.given
import Math.{PI, cos, sin}
import scala.scalajs.js.Date
import scala.scalajs.js.Object.keys
import scala.util.Try

def old_wheely_main_storage_key = "wheely_main_parameter_storage_20022024_wheely"
def wheely_predef_params_storage_key = "wheely_predef_parameter_storage_key_21022024"
def wheely_save_key_prefix = "wheely_params_"

def wheely(e: Event, container: HTMLDivElement): Unit =
  given ctx: CRC2D = makeCanvas(container)
  given cnvs: Canvas = Canvas(ctx, 40)
  given GeoEnv = cnvs.geoEnv
  given center: CanvasCenter = cnvs.center

  if keys(window.localStorage) contains old_wheely_main_storage_key
    then window.localStorage.removeItem(old_wheely_main_storage_key)
  val angle = State(0.0)
  val rodColor = State(RGB.fromCanvasString("#d5ffd2"))
  val rodColorTitle = "Rod Color"

  val rodThickness = State(10)
  val rodThicknessTitle = "Rod Thickness"

  val rodExtremityBigDiskRadius = State(10)
  val rodExtremityBigDiskRadiusTitle = "Rod Extremity Big Radius"

  val rodExtremityBigDiskColor = State(RGB.fromCanvasString("#07ff77"))
  val rodExtremityBigDiskColorTitle = "Rod Extremity Big Disk Color"

  val rodExtremitySmallDiskRadius = State(6)
  val rodExtremitySmallDiskRadiusTitle = "Rod Extremity Small Radius"

  val rodExtremitySmallDiskColor = State(RGB.fromCanvasString("#ffa9e7"))
  val rodExtremitySmallDiskColorTitle = "Rod Extremity Small Disk Color"

  val rodCenterBigDiskRadius = State(9)
  val rodCenterBigDiskRadiusTitle = "Rod Center Big Radius"

  val rodCenterBigDiskColor = State(RGB.fromCanvasString("#000000"))
  val rodCenterBigDiskColorTitle = "Rod Center Big Disk Color"

  val rodCenterSmallDiskRadius = State(3)
  val rodCenterSmallDiskRadiusTitle = "Rod Center Small Radius"
  
  val rodCenterSmallDiskColor = State(RGB.fromCanvasString("#d5ffd2"))
  val rodCenterSmallDiskColorTitle = "Rod Center Small Disk Color"

  val arrowRadius = State(20)
  val arrowRadiusTitle = "Arrow Size"
  
  val arrowAngle = State[Double](2.4)
  val arrowAngleTitle = "Arrow Sharpness"

  val arrowColor = State(RGB.fromCanvasString("#f9dd4b"))
  val arrowColorTitle = "Arrow Color"
  
  val arrowTrailsThicknessStart = State(8)
  val arrowTrailsThicknessStartTitle = "Arrow Trails Thickness Start"
  
  val arrowTrailsThicknessEnd = State(1)
  val arrowTrailsThicknessEndTitle = "Arrow Trails Thickness End"
  
  val arrowTrailLength = State(180)
  val arrowTrailLengthTitle = "Arrow Trails Length"

  val arrowTrailsColorStart = State(RGB.fromCanvasString("#ff7f43"))
  val arrowTrailsColorStartTitle = "Arrow Trails Color Start"
  
  val arrowTrailsColorEnd = State(RGB.fromCanvasString("#fff8b8"))
  val arrowTrailsColorEndTitle = "Arrow Trails Color End"
  
  val arrowPositionOnRod = State(0.25)
  val arrowPositionOnRodTitle = "Arrow Position On Rod"
  
  val circleColor = State(RGB.fromCanvasString("#e5ff96"))
  val circleColorTitle = "Circle Color"
  
  val circleThickness = State(4)
  val circleThicknessTitle = "Circle Thickness"
  
  val axisThickness = State(4)
  val axisThicknessTitle = "Axis Thickness"
  
  val verticalAxisColor = State(RGB.fromCanvasString("#9dff91"))
  val verticalAxisColorTitle = "Vertical Axis Color"
  
  val horizontalAxisColor = State(RGB.fromCanvasString("#9dff91"))
  val horizontalAxisColorTitle = "Horizontal Axis Color"
  
  val controls: Array[State[_ >: I01 with RGB with Int with Double]] = Array(
    rodColor, rodThickness, rodExtremityBigDiskRadius, rodExtremityBigDiskColor,
    rodExtremitySmallDiskRadius, rodExtremitySmallDiskColor, rodCenterBigDiskRadius,
    rodCenterBigDiskColor, rodCenterSmallDiskRadius, rodCenterSmallDiskColor, arrowRadius, arrowAngle,
    arrowColor, arrowTrailsThicknessStart, arrowTrailsThicknessEnd, arrowTrailLength, arrowTrailsColorStart,
    arrowTrailsColorEnd, arrowPositionOnRod, circleColor, circleThickness, axisThickness, verticalAxisColor,
    horizontalAxisColor
  )

  val titles = List(
    rodColorTitle, rodThicknessTitle, rodExtremityBigDiskColorTitle, rodExtremityBigDiskRadiusTitle, 
    rodExtremitySmallDiskColorTitle, rodExtremitySmallDiskRadiusTitle, rodCenterBigDiskColorTitle,
    rodCenterBigDiskRadiusTitle, rodCenterSmallDiskColorTitle, rodCenterSmallDiskRadiusTitle,
    arrowColorTitle, arrowRadiusTitle, arrowAngleTitle, arrowPositionOnRodTitle, arrowTrailsColorStartTitle,
    arrowTrailsThicknessStartTitle, arrowTrailsColorEndTitle, arrowTrailsThicknessEndTitle, arrowTrailLengthTitle,
    circleColorTitle, circleThicknessTitle, horizontalAxisColorTitle, verticalAxisColorTitle, axisThicknessTitle
  )
  
  window.localStorage.setItem(wheely_predef_params_storage_key, controls.map(_.value.toString).mkString(";"))

  val trail1 = StateFrame(arrowTrailLength.value, arrowTrailsThicknessStart.value, arrowTrailsThicknessEnd.value, ColorGradient(arrowTrailsColorStart.value, arrowTrailsColorEnd.value))
  val trail2 = StateFrame(arrowTrailLength.value, arrowTrailsThicknessStart.value, arrowTrailsThicknessEnd.value, ColorGradient(arrowTrailsColorStart.value, arrowTrailsColorEnd.value))

  window.setInterval(() => window.requestAnimationFrame(_ => {
    given cnvs: Canvas = Canvas(ctx, 40)
    given GeoEnv = cnvs.geoEnv
    given center: CanvasCenter = cnvs.center
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height)
    axis(Point(-cnvs.squareSize/2, 0), Point(cnvs.squareSize/2, 0), horizontalAxisColor, axisThickness)
    axis(Point(0, -cnvs.squareSize/2), Point(0, cnvs.squareSize/2), verticalAxisColor, axisThickness)
    circle(cnvs.squareSize / 4, circleColor, circleThickness)
    axis(
      Point(0, (cnvs.squareSize * sin(angle.value) / 2).toInt),
      Point((cnvs.squareSize * cos(angle.value)/ 2).toInt, 0),
      rodColor , rodThickness
    )
    disk(
      Point(0, (cnvs.squareSize * sin(angle.value) / 2).toInt),
      rodExtremityBigDiskRadius,
      rodExtremitySmallDiskRadius,
      rodExtremityBigDiskColor,
      rodExtremitySmallDiskColor
    )
    disk(
      Point((cnvs.squareSize * cos(angle.value)/ 2).toInt, 0),
      rodExtremityBigDiskRadius,
      rodExtremitySmallDiskRadius,
      rodExtremityBigDiskColor,
      rodExtremitySmallDiskColor
    )
    disk(
      Point((cnvs.squareSize * cos(angle.value)/ 4).toInt, (cnvs.squareSize * sin(angle.value) / 4).toInt),
      rodCenterBigDiskRadius,
      rodCenterSmallDiskRadius,
      rodCenterBigDiskColor,
      rodCenterSmallDiskColor
    )
    val mark1 = Point(
      (arrowPositionOnRod.value * cnvs.squareSize * cos(angle.value)/ 2).toInt,
      ((1 - arrowPositionOnRod.value) * cnvs.squareSize * sin(angle.value) / 2).toInt
    )
    val mark2 = Point(
      ((1 - arrowPositionOnRod.value) * cnvs.squareSize * cos(angle.value)/ 2).toInt,
      (arrowPositionOnRod.value * cnvs.squareSize * sin(angle.value) / 2).toInt
    )
    val colorGradient = ColorGradient(arrowTrailsColorStart.value, arrowTrailsColorEnd.value)
    trail1.resolution = arrowTrailLength.value
    trail1.startThickness = arrowTrailsThicknessStart.value
    trail1.endThickness = arrowTrailsThicknessEnd.value
    trail1.colorGradient = colorGradient
    trail2.resolution = arrowTrailLength.value
    trail2.startThickness = arrowTrailsThicknessStart.value
    trail2.endThickness = arrowTrailsThicknessEnd.value
    trail2.colorGradient = colorGradient
    trail1.addPoint(mark1)
    trail2.addPoint(mark2)
    cnvs.renderFrames(trail1.getFrame, trail2.getFrame)
    arrow(mark1, arrowRadius, arrowAngle, arrowColor, arrowPositionOnRod.value)
    arrow(mark2, arrowRadius, arrowAngle, arrowColor, 1 - arrowPositionOnRod.value)
    angle.value += PI / 100
    if angle.value > 2 * PI then angle.value -= 2 * PI
  }), 10)

  val editingPanel = createEditingPanel(container)
  def pickRightFont = () => {
    val tmp = document.createElement("div").asInstanceOf[HTMLDivElement]
    tmp.style.display = "block"
    tmp.style.position = "absolute"
    tmp.style.left = "-1000px"
    val longest = titles.map(t => (t.length, t)).max._2
    tmp.innerText = longest
    editingPanel.style.fontSize = "30px"
    editingPanel.appendChild(tmp)
    while tmp.clientWidth > editingPanel.clientWidth * 0.9 do
      val sz = editingPanel.style.fontSize.stripSuffix("px").toInt
      editingPanel.style.fontSize = s"${(sz * 0.9).toInt}px"
    editingPanel.removeChild(tmp)
  }
  window.setTimeout(pickRightFont, 10)
  window.addEventListener("resize", _ => pickRightFont())
  
  container.appendChild(editingPanel)
  addColorEditor(editingPanel, rodColorTitle, rodColor)
  addEditor(editingPanel, rodThicknessTitle, rodThickness, extractInt)
  addColorEditor(editingPanel, rodExtremityBigDiskColorTitle, rodExtremityBigDiskColor)
  addEditor(editingPanel, rodExtremityBigDiskRadiusTitle, rodExtremityBigDiskRadius, extractInt)
  addColorEditor(editingPanel, rodExtremitySmallDiskColorTitle, rodExtremitySmallDiskColor)
  addEditor(editingPanel, rodExtremitySmallDiskRadiusTitle, rodExtremitySmallDiskRadius, extractInt)
  addColorEditor(editingPanel, rodCenterBigDiskColorTitle, rodCenterBigDiskColor)
  addEditor(editingPanel, rodCenterBigDiskRadiusTitle, rodCenterBigDiskRadius, extractInt)
  addColorEditor(editingPanel, rodCenterSmallDiskColorTitle, rodCenterSmallDiskColor)
  addEditor(editingPanel, rodCenterSmallDiskRadiusTitle, rodCenterSmallDiskRadius, extractInt)
  addColorEditor(editingPanel, arrowColorTitle, arrowColor)
  addEditor(editingPanel, arrowRadiusTitle, arrowRadius, extractInt)
  addEditor(editingPanel, arrowAngleTitle, arrowAngle, extractDouble)
  addEditor(editingPanel, arrowPositionOnRodTitle, arrowPositionOnRod, extractDouble)
  addColorEditor(editingPanel, arrowTrailsColorStartTitle, arrowTrailsColorStart)
  addEditor(editingPanel, arrowTrailsThicknessStartTitle, arrowTrailsThicknessStart, extractInt)
  addColorEditor(editingPanel, arrowTrailsColorEndTitle, arrowTrailsColorEnd)
  addEditor(editingPanel, arrowTrailsThicknessEndTitle, arrowTrailsThicknessEnd, extractInt)
  addEditor(editingPanel, arrowTrailLengthTitle, arrowTrailLength, extractInt)
  addColorEditor(editingPanel, circleColorTitle, circleColor)
  addEditor(editingPanel, circleThicknessTitle, circleThickness, extractInt)
  addColorEditor(editingPanel, horizontalAxisColorTitle, horizontalAxisColor)
  addColorEditor(editingPanel, verticalAxisColorTitle, verticalAxisColor)
  addEditor(editingPanel, axisThicknessTitle, axisThickness, extractInt)

  addSaveLink(editingPanel, controls)
  addRetrieveLinks(editingPanel, controls)
  addLinkToRemoveAllSavedData(editingPanel, controls)
  addSpaceAtBottom(editingPanel)

def arrow(
  ctr: Point,
  size: State[Int],
  sharpness: State[Double],
  color: State[RGB],
  a: Double
)(using ctx: CRC2D, center: CanvasCenter, geoEnv: GeoEnv): Unit =
  val angle2 = atan2(- ctr.x * (1-a) * (1-a), ctr.y * a * a) + PI
  val angle1 = angle2 + sharpness.value
  val angle3 = angle2 - sharpness.value
  val x = ctr.x + center.x
  val y = center.y - ctr.y
  def corner(angle: Double): Point = Point((x + size.value * cos(angle)).toInt, (y - size.value * sin(angle)).toInt)
  val corner1 = corner(angle1)
  val corner2 = corner(angle2)
  val corner3 = corner(angle3)
  ctx.beginPath()
  ctx.fillStyle = color.value.toCanvasString
  ctx.strokeStyle = color.value.toCanvasString
  ctx.lineWidth = 1
  ctx.moveTo(x, y)
  ctx.lineTo(corner1.x, corner1.y)
  ctx.lineTo(corner2.x, corner2.y)
  ctx.lineTo(corner3.x, corner3.y)
  ctx.closePath()
  ctx.fill()
  ctx.stroke()

def disk(
  center: Point,
  bigRadius: State[Int],
  smallRadius: State[Int],
  bigColor: State[RGB],
  smallColor: State[RGB]
)(using ctx: CRC2D, cvsCenter: CanvasCenter): Unit =
  def singleDisk(radius: State[Int], color: State[RGB]): Unit =
    ctx.moveTo(center.x + cvsCenter.x, cvsCenter.y - center.y)
    ctx.beginPath()
    ctx.lineWidth = 1
    ctx.fillStyle = color.value.toCanvasString
    ctx.strokeStyle = color.value.toCanvasString
    ctx.arc(center.x + cvsCenter.x, cvsCenter.y - center.y, radius.value, 0, 2 * PI, true)
    ctx.stroke()
    ctx.fill()
    ctx.closePath()
  singleDisk(bigRadius, bigColor)
  singleDisk(smallRadius, smallColor)

def circle(
  radius: Int,
  color: State[RGB],
  thickness: State[Int]
)(using ctx: CRC2D, center: CanvasCenter): Unit =
  ctx.beginPath()
  ctx.strokeStyle = color.value.toCanvasString
  ctx.lineWidth = thickness.value
  ctx.ellipse(center.x, center.y, radius, radius, 0, 0, 2 * PI)
  ctx.closePath()
  ctx.stroke()

def axis(
  fromPoint: Point,
  toPoint: Point,
  color: State[RGB],
  thickness: State[Int]
)(using ctx: CRC2D, center: CanvasCenter): Unit =
  ctx.beginPath()
  ctx.moveTo(fromPoint.x + center.x, center.y - fromPoint.y)
  ctx.strokeStyle = color.value.toCanvasString
  ctx.lineWidth = thickness.value
  ctx.lineTo(toPoint.x + center.x, center.y - toPoint.y)
  ctx.stroke()
  ctx.closePath()

def createEditingPanel(
  container: HTMLDivElement
)(using ctx: CRC2D, center: CanvasCenter, ge: GeoEnv): HTMLDivElement =
  val elem = document.createElement("div").asInstanceOf[HTMLDivElement]
  val cvs = document.getElementById("maincanvas").asInstanceOf[HTMLCanvasElement]
  elem.id = "parameditor"
  elem.style.position = "fixed"
  val setSizes = () => {
    val ctx = cvs.getContext("2d").asInstanceOf[CRC2D]
    elem.style.top = s"${cvs.getBoundingClientRect().bottom + 10}px"
    elem.style.height = s"${window.innerHeight -  cvs.getBoundingClientRect().bottom - 20}px"
    val maxWidth = cvs.style.width.stripSuffix("px").toInt
    val width = (maxWidth * 0.95).toInt
    elem.style.width = s"${width}px"
    elem.style.left = s"${(window.innerWidth - width) / 2}px"
  }
  window.setTimeout(setSizes, 10)
  window.addEventListener("resize", _ => setSizes())
  elem

def addEditor[T](container: HTMLDivElement, title: String, state: State[T], extractVal: String => T): Unit =
  val envelope = document.createElement("div").asInstanceOf[HTMLDivElement]
  envelope.style.border = "1px #4080ff solid"
  envelope.style.margin = "20px"
  container.appendChild(envelope)
  val titleDiv = document.createElement("div").asInstanceOf[HTMLDivElement]
  titleDiv.innerText = title
  titleDiv.style.textAlign = "center"
  titleDiv.style.backgroundColor = "steelblue"
  titleDiv.style.color = "cyan"
  envelope.appendChild(titleDiv)
  val valueDiv = document.createElement("div").asInstanceOf[HTMLDivElement]
  var v = state.value.toString
  val f = v.contains('.')
  if f then v = f"${state.value.asInstanceOf[Double]}%2.1f"
  valueDiv.innerText = v
  valueDiv.style.backgroundColor = "#4b5382"
  valueDiv.style.color = "cyan"
  valueDiv.style.textAlign = "center"
  valueDiv.contentEditable = "true"
  valueDiv.style.border = "grey 1px solid"
  envelope.appendChild(valueDiv)
  state.updateUI = Some(v => valueDiv.innerText = state.value.toString)
  valueDiv.addEventListener("keydown", (ev: Event) => {
    val k = ev.asInstanceOf[KeyboardEvent].keyCode
    if (k < 48 || k > 57) && k != 190 && k != 8 && k != 37 && k != 39
    then
      ev.stopImmediatePropagation()
      ev.preventDefault()
  })
  valueDiv.addEventListener("input", _ => {
    state.value = extractVal(valueDiv.innerText)
  })

def extractInt(s: String): Int =
  val x = Try(s.toInt).getOrElse(2)
  if x < 2 then 2 else x

def extractDouble(s: String): Double =
  val x = Try(s.toDouble).getOrElse(1.0)
  if x < 0.1 then 0.1 else x + 0.00001

def addColorEditor(container: HTMLDivElement, title: String, state: State[RGB]): Unit =
  val envelope = document.createElement("div").asInstanceOf[HTMLDivElement]
  envelope.style.border = "1px #4080ff solid"
  envelope.style.margin = "20px"
  envelope.style.textAlign = "center"
  container.appendChild(envelope)
  val titleDiv = document.createElement("div").asInstanceOf[HTMLDivElement]
  titleDiv.innerText = title
  titleDiv.style.textAlign = "center"
  titleDiv.style.backgroundColor = "steelblue"
  titleDiv.style.color = "cyan"
  envelope.appendChild(titleDiv)
  val valueDiv = document.createElement("input").asInstanceOf[Input]
  valueDiv.value = state.value.toCanvasString
  valueDiv.`type` = "color"
  valueDiv.style.textAlign = "center"
  valueDiv.style.backgroundColor = "transparent"
  valueDiv.style.margin = "0px"
  window.setTimeout(() => {valueDiv.style.margin = "0px"}, 10)
  envelope.appendChild(valueDiv)
  val stringValDiv = document.createElement("div").asInstanceOf[HTMLDivElement]
  stringValDiv.innerText = state.value.toCanvasString
  stringValDiv.style.backgroundColor = "#4b5382"
  stringValDiv.style.color = "cyan"
  stringValDiv.style.textAlign = "center"
  stringValDiv.contentEditable = "true"
  stringValDiv.style.border = "grey 1px solid"
  envelope.appendChild(stringValDiv)
  state.updateUI = Some(v => {
    valueDiv.value = v.toCanvasString
    stringValDiv.innerText = valueDiv.value
  })
  valueDiv.addEventListener(
    "input", _ => {
      state.value = RGB.fromCanvasString(valueDiv.value)
      stringValDiv.innerText = valueDiv.value
  })
  window.setTimeout(() => {
    valueDiv.style.marginLeft = s"${(envelope.clientWidth - valueDiv.clientWidth) / 2}px"
  }, 5)
  stringValDiv.addEventListener( "input", _ => {
    if
      stringValDiv.innerText.length == 7 &&
      stringValDiv.innerText(0) == '#' &&
      stringValDiv.innerText.substring(1).forall { c =>
        ('0' <= c && c <= '9') || ('a' <= c.toLower && c.toLower <= 'f')
      }
    then
      state.value = RGB.fromCanvasString(stringValDiv.innerText)
      valueDiv.value = stringValDiv.innerText
  })

def addSaveLink(
  editingPanel: HTMLDivElement,
  controls: Array[State[_ >: I01 with RGB with Int with Double]]
): Unit =
  val l = document.createElement("a").asInstanceOf[HTMLAnchorElement]
  l.textContent = "Save current param set"
  l.href = "#"
  l.style.color = "cyan"
  l.style.backgroundColor = "transparent"
  l.style.display = "block"
  l.style.textAlign = "center"
  l.style.margin = "auto"
  l.style.padding = "15px"
  editingPanel.appendChild(l)
  l.addEventListener("click", _ => {
    val s = controls.map(_.value.toString).mkString(";")
    window.navigator.clipboard.writeText(s)
    window.localStorage.setItem(wheely_save_key_prefix + Date.now(), s)
  })

def addRetrieveLinks(
  editingPanel: HTMLDivElement,
  controls: Array[State[_ >: I01 with RGB with Int with Double]]
): Unit =
  val envelope = document.createElement("div").asInstanceOf[HTMLDivElement]
  envelope.style.margin = "20px"
  editingPanel.appendChild(envelope)
  window.setInterval( () => {
    val retrieveKeys = keys(window.localStorage).filter(_.startsWith(wheely_save_key_prefix)).sorted.reverse
    if retrieveKeys.length != envelope.childElementCount
      then
        while envelope.childElementCount > 0 do envelope.firstElementChild.remove()
        retrieveKeys.foreach { k =>
          val ts = new Date(k.stripPrefix(wheely_save_key_prefix).stripMargin.toDouble)
          val l = document.createElement("a").asInstanceOf[HTMLAnchorElement]
          l.textContent = "Retrieve " + ts.toLocaleString().filterNot(_ == ',')
          l.href = "#"
          l.style.color = "cyan"
          l.style.backgroundColor = "transparent"
          l.style.display = "block"
          l.style.textAlign = "center"
          l.style.margin = "auto"
          l.style.padding = "15px"
          envelope.appendChild(l)
          l.addEventListener(
            "click", _ => {
              val s = window.localStorage.getItem(k)
              window.navigator.clipboard.writeText(s)
              controls.zip(s.split(";")).foreach { (c, v) =>
                c.fromString(v)
                c.updateUI.foreach(_(c.value))
              }
            }
          )
        }
  }, 1000)

def addLinkToRemoveAllSavedData(
  editingPanel: HTMLDivElement,
  controls: Array[State[_ >: I01 with RGB with Int with Double]]
): Unit =
  val envelope = document.createElement("div").asInstanceOf[HTMLDivElement]
  envelope.style.margin = "20px"
  editingPanel.appendChild(envelope)
  {
    val l = document.createElement("a").asInstanceOf[HTMLAnchorElement]
    l.textContent = "Discard all saved parameter sets"
    l.href = "#"
    l.style.color = "cyan"
    l.style.backgroundColor = "transparent"
    l.style.display = "block"
    l.style.textAlign = "center"
    l.style.margin = "auto"
    l.style.padding = "15px"
    envelope.appendChild(l)
    l.addEventListener(
      "click", _ => {
        val retrieveKeys = keys(window.localStorage).filter(_.startsWith(wheely_save_key_prefix))
        retrieveKeys.foreach {window.localStorage.removeItem(_)}
      }
    )
  }
  {
    val l = document.createElement("a").asInstanceOf[HTMLAnchorElement]
    l.textContent = "Reload predefined params"
    l.href = "#"
    l.style.color = "cyan"
    l.style.backgroundColor = "transparent"
    l.style.display = "block"
    l.style.textAlign = "center"
    l.style.margin = "auto"
    l.style.padding = "15px"
    envelope.appendChild(l)
    l.addEventListener(
      "click", _ => {
        if keys(window.localStorage) contains wheely_predef_params_storage_key
        then
          val s = window.localStorage.getItem(wheely_predef_params_storage_key)
          window.navigator.clipboard.writeText(s)
          controls.zip(s.split(";")).foreach { (c, v) =>
            c.fromString(v)
            c.updateUI.foreach(_(c.value))
          }
      }
    )
  }

def addSpaceAtBottom(container: HTMLDivElement): Unit =
  val envelope = document.createElement("div").asInstanceOf[HTMLDivElement]
  container.appendChild(envelope)
  val titleDiv = document.createElement("div").asInstanceOf[HTMLDivElement]
  titleDiv.innerText = "\uD83D\uDCA2\uD83D\uDCA2\uD83D\uDCA2\uD83D\uDCA2\uD83D\uDCA2"
  titleDiv.style.textAlign = "center"
  titleDiv.style.backgroundColor = "transparent"
  envelope.appendChild(titleDiv)