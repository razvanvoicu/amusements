package sg.raz.www.js

import Math.{PI, sin, cos, atan2, pow}

package object geometry:
  opaque type CanvasColor = String

  extension(c: CanvasColor)
    def asString: String = c

  object CanvasColor:
    def apply(c: RGB): CanvasColor = c.toCanvasString

  opaque type I01 = Double

  extension(v: I01)
    def +(w: Double): Double = v + w
    def -(w: Double): Double = v + w
    def *(w: Double): Double = v + w
    def /(w: Double): Double = v + w
    def <(w: I01): Boolean = v < w
    def <=(w: I01): Boolean = v <= w
    def >(w: I01): Boolean = v > w
    def >=(w: I01): Boolean = v >= w

  object I01:
    def apply(value: Double): I01 =
      assert(0.0 <= value && value <= 1.0, "Expecting positive sub-unit value")
      value

  opaque type Curve = I01 => Point

  extension(curve: Curve)
    def firstPoint: Point = curve(0)
    def discretize(n: Int): Iterable[Point] = ((0 until n) map (k => curve(1.0/n * k))).to(Iterable)
    def rotate(radians: Double): Curve = curve andThen (_.rotate(radians))
    def translate(distX: Int, distY: Int): Curve = curve andThen (_.translate(distX, distY))
    def scale(factor: Double): Curve = curve andThen (_.scale(factor))
    def invert: Curve = (1.0 - (_:Double)) andThen curve
    def truncate(start: I01, end: I01): Curve = (t: I01) => curve(start + t*(end-start))
    def trim(start:I01, end: I01)(using ge: GeoEnv): Curve =
      case _ @ t if t < start || t > end => Point(0, 0)
      case _ @ t => curve(t)
    def moveStart(k: I01)(using ge: GeoEnv): Curve =
      concatCurvesWithSplits(
        Array(1-k, I01(1.0)),
        Array(curve.truncate(k, I01(1.0)), curve.truncate(I01(0.0), k))
      )

  object Curve:
    def apply(f: Double => Point): Curve = f

  def concatCurves(curves: Curve*): Curve =
    val cA = curves.toArray
    val n = cA.length
    Curve (
      t =>
        val k = (n * t).toInt
        val r = n * t - k
        cA(k)(I01(r))
    )
  def concatCurvesWithSplits(splits: Array[I01], curves: Array[Curve]): Curve =
    assert(splits.length == curves.length)
    Curve (
      t =>
        val k = splits.indexWhere(_ >= t)
        val range = splits(k) - (if k == 0 then 0.0 else splits(k-1))
        val r = t - (if k == 0 then 0.0 else splits(k-1))
        curves(k)(I01(r / range))
    )
    
  def morphCurves(curve1: Curve, curve2: Curve, morphState: I01)(using ge: GeoEnv): Curve = Curve { t => 
    val p1 = curve1(t)
    val p2 = curve2(t)
    Point((p1.x * (1-morphState) + p2.x * morphState).toInt, (p1.y * (1-morphState) + p2.y * morphState).toInt)
  }
  
  def threadCurves(curve1: Curve, curve2: Curve, progress: Double)(using ge: GeoEnv): Curve = Curve { t => () match {
    case _ if t < progress => curve2(t)
    case _ if t > progress + 0.1 => curve1(t)
    case _ =>
      val ratio = (t-progress) * 10
      val p1 = curve1(t)
      val p2 = curve2(t)
      Point((p1.x * ratio + p2.x * (1-ratio)).toInt, (p1.y * ratio + p2.y * (1-ratio)).toInt)
  }}

  enum OpenOrClosedPath:
    case OpenPath
    case ClosedPath

  def unitSeg(using ge: GeoEnv): Curve = (t: I01) => Point((t * ge.squareSize / 2).toInt, 0)
  def unitCircle(using ge: GeoEnv): Curve =
    (t: I01) => Point(
      (ge.squareSize * cos(t * PI * 2) / 2).toInt,
      (ge.squareSize * sin(t * PI * 2) / 2).toInt
    )

  def seg(p1: Point, p2: Point): Curve = Curve(p1.segmentTo(p2))
    
  def polygon(n: Int)(using ge: GeoEnv): Curve =
    val firstSide = unitSeg
      .translate(- ge.squareSize / 4, 0)
      .scale(2 * sin(PI / n))
      .translate(0, - (ge.squareSize * cos(PI / n) / 2).toInt)
    val angles = Iterator.iterate(0.0, n)(_ + 2*PI / n)
    val sides = angles.map(a => firstSide.rotate(a)).toSeq
      //Iterator.iterate(firstSide, n)(_.rotate(2 * PI / n)).toSeq
    concatCurves(sides: _*)
    
  def triangle(using ge: GeoEnv): Curve = polygon(3)
  def square(using ge: GeoEnv): Curve = polygon(4)
  def pentagon(using ge: GeoEnv): Curve = polygon(5)
  def hexagon(using ge: GeoEnv): Curve = polygon(6)
  def heptagon(using ge: GeoEnv): Curve = polygon(7)
  def octagon(using ge: GeoEnv): Curve = polygon(8)
  def p9(using ge: GeoEnv): Curve = polygon(9)
  def p10(using ge: GeoEnv): Curve = polygon(10)
  def p11(using ge: GeoEnv): Curve = polygon(11)
  def p12(using ge: GeoEnv): Curve = polygon(12)

  def star(sides: Int, skip: Int)(using ge: GeoEnv): Curve =
    assert(sides > 2 && skip > 0 && skip <= sides / 2 && sides % skip != 0)
    val angleOffset = PI/2
    val skipAngle = skip * 2 * PI / sides
    val firstVertex = Point(
      (ge.squareSize * cos(angleOffset)/ 2).toInt,
      (ge.squareSize * sin(angleOffset)/ 2).toInt
    )
    val secondVertex = Point(
      (ge.squareSize * cos(angleOffset + skipAngle)/ 2).toInt,
      (ge.squareSize * sin(angleOffset + skipAngle)/ 2).toInt
    )
    val firstSide = seg(firstVertex, secondVertex)
    val angles = Iterator.iterate(0.0, sides)(_ + skipAngle).toSeq
    val allSides = angles.map(a => firstSide.rotate(a))
      //Iterator.iterate(firstSide, sides)(_.rotate(skipAngle)).toSeq
    concatCurves(allSides: _*)

  def octogram(using ge: GeoEnv): Curve = concatCurves(square.moveStart(0.0625), square.rotate(PI/4).moveStart(0.9375))
  def nonagram(using ge: GeoEnv): Curve =
    val s = sin(2 * PI / 9) / (cos(PI / 9) * 6 * cos(PI / 6))
    concatCurvesWithSplits(
      Array(I01(9.0/27), I01(17.0/27), I01(26.0/27), I01(1.0)),
      Array(
        triangle.moveStart(I01(s)),
        triangle.rotate(- PI * 2.0 / 9).moveStart(I01(1.0/3 - s)).truncate(I01(0.0), I01(2.0/3+2*s)),
        triangle.rotate(- PI * 4.0 / 9).moveStart(I01(1.0/3 - s)),
        triangle.rotate(- PI * 2.0 / 9).moveStart(I01(1.0/3 - s)).truncate(I01(2.0/3+2*s), I01(1.0))
      )
    )

  def starDavid(using ge: GeoEnv): Curve =
    concatCurves(
      polygon(3).moveStart(1.0/9),
      polygon(3).rotate(-Math.PI / 3).moveStart(2.0/9)
    )


  def threeCircles(using ge: GeoEnv): Curve =
    val radius = ge.squareSize / 2
    val halfCirc = unitCircle.scale(0.5)
    val circTop = halfCirc.translate(0, radius/2).moveStart(2.0/3)
    val circTopSmall = circTop.truncate(I01(0.0), I01(1.0/6))
    val circTopBig = circTop.truncate(I01(1.0/6), I01(1.0)).invert
    val slideDown = -(radius * (cos(PI / 6) - 0.5)).toInt
    val circBotLeft = halfCirc.translate(-radius/2, slideDown)
    val circBotLeftSmall = circBotLeft.truncate(I01(0.0), I01(1.0/6))
    val circBotLeftBig = circBotLeft.truncate(I01(1.0/6), I01(1.0)).invert
    val circBotRight = halfCirc.translate(+radius / 2, slideDown).moveStart(1.0/3)
    val circBotRightSmall = circBotRight.truncate(I01(0.0), I01(1.0/6))
    val circBotRightBig = circBotRight.truncate(I01(1.0/6), I01(1.0)).invert
    concatCurvesWithSplits(
      Array(5,6,11,12,17,18).map(k => I01(k/18.0)),
      Array(circTopBig, circBotRightSmall, circBotLeftBig, circTopSmall, circBotRightBig, circBotLeftSmall)
    )

  def fourCirclesSquare(using ge: GeoEnv): Curve =
    val halfCirc = unitCircle.scale(0.5)
    val threeQuarters = halfCirc.truncate(I01(0.0), I01(0.75))
    val oneQuarter = halfCirc.truncate(I01(0.75), I01(1.0))
    val nwL = threeQuarters.translate(- ge.squareSize / 4, ge.squareSize / 4)
    val nwS = oneQuarter.translate(- ge.squareSize / 4, ge.squareSize / 4)
    val neL = threeQuarters.rotate(-PI/2).translate(ge.squareSize / 4, ge.squareSize / 4).invert
    val neS = oneQuarter.rotate(-PI/2).translate(ge.squareSize / 4, ge.squareSize / 4).invert
    val swL = threeQuarters.rotate(PI/2).translate(-ge.squareSize / 4, -ge.squareSize / 4).invert
    val swS = oneQuarter.rotate(PI/2).translate(-ge.squareSize / 4, -ge.squareSize / 4).invert
    val seL = threeQuarters.rotate(PI).translate(ge.squareSize / 4, -ge.squareSize / 4)
    val seS = oneQuarter.rotate(PI).translate(ge.squareSize / 4, -ge.squareSize / 4)
    concatCurvesWithSplits(
      Array(3, 4, 7, 8, 11, 12, 15, 16).map(k => I01(k / 16.0)),
      Array(nwL, swS, seL, neS, neL, seS, swL, nwS)
    )

  def sevenCircles(using ge: GeoEnv): Curve =
    val qcirc = unitCircle.scale(0.25).translate(-ge.squareSize/4, 0)
    val arc = unitCircle.scale(0.25).truncate(I01(1.0/3), I01(1.0/2)).invert
    val frag = concatCurvesWithSplits(Array(6, 7).map(k => I01(k/7.0)), Array(qcirc, arc))
    concatCurves(Iterator.iterate(frag, 6)(_.rotate(-PI/3)).toSeq: _*)

  def spiral(using ge: GeoEnv): Curve =
    concatCurves(
      unitCircle.truncate(I01(0.0), I01(0.5)),
      unitCircle.scale(0.8).truncate(I01(0.5), I01(1.0)).translate(- ge.squareSize / 10, 0),
      unitCircle.scale(0.6).truncate(I01(0.0), I01(0.5)),
      unitCircle.scale(0.4).truncate(I01(0.5), I01(1.0)).translate(- ge.squareSize / 10, 0),
      unitCircle.scale(0.05).truncate(I01(0.0), I01(0.5)).translate( 3 * ge.squareSize / 40, 0),
      unitCircle.scale(0.3).truncate(I01(0.5), I01(1.0)).invert.translate(- ge.squareSize / 10, 0),
      unitCircle.scale(0.5).truncate(I01(0.0), I01(0.5)).invert,
      unitCircle.scale(0.7).truncate(I01(0.5), I01(1.0)).invert.translate(- ge.squareSize / 10, 0),
      unitCircle.scale(0.9).truncate(I01(0.0), I01(0.5)).invert,
      unitCircle.scale(0.05).truncate(I01(0.5), I01(1.0)).translate( 19 * ge.squareSize / 40, 0),
    )

  def spiral2(using ge: GeoEnv): Curve =
    concatCurves(
      unitCircle.truncate(I01(0.0), I01(0.5)),
      unitCircle.scale(0.9).truncate(I01(0.5), I01(1.0)).translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.8).truncate(I01(0.0), I01(0.5)),
      unitCircle.scale(0.7).truncate(I01(0.5), I01(1.0)).translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.6).truncate(I01(0.0), I01(0.5)),
      unitCircle.scale(0.5).truncate(I01(0.5), I01(1.0)).translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.4).truncate(I01(0.0), I01(0.5)),
      unitCircle.scale(0.3).truncate(I01(0.5), I01(1.0)).translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.05).truncate(I01(0.0), I01(0.5)).translate(3 * ge.squareSize / 40, 0),
      unitCircle.scale(0.2).truncate(I01(0.5), I01(1.0)).invert.translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.3).truncate(I01(0.0), I01(0.5)).invert,
      unitCircle.scale(0.4).truncate(I01(0.5), I01(1.0)).invert.translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.5).truncate(I01(0.0), I01(0.5)).invert,
      unitCircle.scale(0.6).truncate(I01(0.5), I01(1.0)).invert.translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.7).truncate(I01(0.0), I01(0.5)).invert,
      unitCircle.scale(0.8).truncate(I01(0.5), I01(1.0)).invert.translate(-ge.squareSize / 20, 0),
      unitCircle.scale(0.9).truncate(I01(0.0), I01(0.5)).invert,
      unitCircle.scale(0.05).truncate(I01(0.5), I01(1.0)).translate(19 * ge.squareSize / 40, 0),
    )

  def zigzag(c1: Curve, c2: Curve, n: Int): Curve = Curve ( t =>
    val k = 2 * n * t
    (k.toInt, k - k.toInt) match {
      case (k, r) if k % 2 == 0 => c1(t).segmentTo(c2(t))(r)
      case (_, r) => c2(t).segmentTo(c1(t))(r)
    }
  )

  def mix(c1: Curve, c2: Curve, mixer: Double => Double): Curve =
    Curve( t => c1(t).segmentTo(c2(t))(mixer(t)) )

  def interleave(c1: Curve, c2: Curve, n: Int): Curve = (t: Double) => (2 * n * t).toInt match {
    case k if k % 2 == 0 => c1(t)
    case k if k % 2 != 0 => c2(t)
  }

  def ellipse(using ge: GeoEnv): Curve = Curve (
    t => Point((ge.squareSize * cos(2*PI*t) / 2).toInt, (ge.squareSize * sin(2*PI*t) / 4).toInt)
  )
  
  def ellipseXY(x: I01, y: I01)(using ge: GeoEnv): Curve = Curve (
    t => Point((x * ge.squareSize * cos(2*PI*t) / 2).toInt, (y * ge.squareSize * sin(2*PI*t) / 2).toInt)
  )

  def interEllipse(using ge: GeoEnv): Curve = interleave(ellipse, ellipse.scale(0.5), 8)

  def zigZagEllipse(using ge: GeoEnv): Curve= zigzag(ellipse, ellipse.scale(0.5), 8)

  def sinEllipse(using ge: GeoEnv): Curve = mix(ellipse, ellipse.scale(0.5), t => (1 + sin(8 * PI * t))/2)

  def inter1(using ge: GeoEnv): Curve = interleave(unitCircle, square.scale(0.5).rotate(3*PI/4), 20)
  def inter2(using ge: GeoEnv): Curve = interleave(triangle.scale(0.5).rotate(PI/6), octagon, 10)
  def cogStar4(using ge: GeoEnv): Curve = zigzag(unitCircle, unitCircle.scale(0.5), 4)
  def cogStar32(using ge: GeoEnv): Curve = zigzag(unitCircle, unitCircle.scale(0.8), 32)
  def sinMix4(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.3),
    x => sin(8 * PI * x)
  )
  def sinPosMix4(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.3),
    x => (1 + sin(8 * PI * x)) / 2
  )
  def sin2PosMix4(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.3),
    x => pow((1 + sin(8 * PI * x)) / 2, 2)
  )
  def sinMix6(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.4),
    x => sin(12 * PI * x)
  )
  def sinPosMix6(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.4),
    x => (1 + sin(12 * PI * x)) / 2
  )
  def sinMix16(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.4),
    x => sin(32 * PI * x)
  )
  def sinPosMix16(using ge: GeoEnv): Curve = mix(
    unitCircle.scale(0.7),
    unitCircle.scale(0.3),
    x => (1 + sin(32 * PI * x)) / 2
  )
  def ellipseMix4(using ge: GeoEnv): Curve = mix(
    ellipseXY(I01(0.4), I01(0.7)),
    ellipseXY(I01(0.7), I01(0.4)),
    x => sin(8*PI*x)
  )
  def ellipseMix6(using ge: GeoEnv): Curve = mix(
    ellipseXY(I01(0.4), I01(0.7)),
    ellipseXY(I01(0.7), I01(0.4)),
    x => sin(12*PI*x)
  )
  def ellipse2Mix4(using ge: GeoEnv): Curve = mix(
    ellipseXY(I01(0.4), I01(0.8)),
    ellipseXY(I01(0.8), I01(0.4)),
    x =>  (1 + sin(8 * PI * x)) / 2
  )
  def ellipse2Mix6(using ge: GeoEnv): Curve = mix(
    ellipseXY(I01(0.4), I01(0.8)),
    ellipseXY(I01(0.8), I01(0.4)),
    x =>  (1 + sin(12 * PI * x)) / 2
  )
  def ellipse2Mix8(using ge: GeoEnv): Curve = mix(
    ellipseXY(I01(0.4), I01(0.8)),
    ellipseXY(I01(0.8), I01(0.4)),
    x =>  (1 + sin(16 * PI * x)) / 2
  )
  def eccentricCogs(using ge: GeoEnv): Curve = zigzag(
    unitCircle, 
    unitCircle.scale(0.4).translate(ge.squareSize/4, 0),
    16
  )
  def maltaCross(using ge: GeoEnv): Curve = zigzag(
    ellipseXY(I01(0.5), I01(0.75)),
    ellipseXY(I01(0.75), I01(0.5)),
    6
  )

  def curves(using ge: GeoEnv): Array[(String, Curve)] = Array(
    "starDavid"         -> starDavid,
    "unitCircle"        -> unitCircle,
    "nonagram"          -> nonagram,
    "octogram"          -> octogram,
    "triangle"          -> triangle,
    "square"            -> square,
    "polygon5"          -> polygon(5),
    "polygon6"          -> polygon(6),
    "polygon7"          -> polygon(7),
    "polygon8"          -> polygon(8),
    "polygon9"          -> polygon(9),
    "polygon10"         -> polygon(10),
    "polygon11"         -> polygon(11),
    "polygon12"         -> polygon(12),
    "polygon13"         -> polygon(13),
    "star5-2"           -> star(5, 2),
    "star7-3"           -> star(7, 3),
    "star7-2"           -> star(7, 2),
    "star9-4"           -> star(9, 4),
    "star9-2"           -> star(9, 2),
    "star11-5"          -> star(11, 5),
    "star11-4"          -> star(11, 4),
    "star11-3"          -> star(11, 3),
    "star11-2"          -> star(11, 2),
    "star13-6"          -> star(13, 6),
    "star13-5"          -> star(13, 5),
    "star13-4"          -> star(13, 4),
    "star13-3"          -> star(13, 3),
    "star13-2"          -> star(13, 2),
    "inter1"            -> inter1,
    "inter2"            -> inter2,
    "cogStar4"          -> cogStar4,
    "cogStar32"         -> cogStar32,
    "sinMix4"           -> sinMix4,
    "sinPosMix4"        -> sinPosMix4,
    "sin2PosMix4"       -> sin2PosMix4,
    "sinMix6"           -> sinMix6,
    "sinPosMix6"        -> sinPosMix6,
    "sinMix16"          -> sinMix16,
    "sinPosMix16"       -> sinPosMix16,
    "ellipseMix4"       -> ellipseMix4,
    "ellipseMix6"       -> ellipseMix6,
    "ellipse2Mix4"      -> ellipse2Mix4,
    "ellipse2Mix6"      -> ellipse2Mix6,
    "ellipse2Mix8"      -> ellipse2Mix8,
    "eccentricCogs"     -> eccentricCogs,
    "maltaCross"        -> maltaCross,
    "sinEllipse"        -> sinEllipse,
    "zigZagEllipse"     -> zigZagEllipse,
    "interEllipse"      -> interEllipse,
    "ellipse"           -> ellipse,
    "spiral"            -> spiral,
    "spiral2"           -> spiral2,
    "sevenCircles"      -> sevenCircles,
    "fourCirclesSquare" -> fourCirclesSquare,
    "threeCircles"      -> threeCircles,
  )