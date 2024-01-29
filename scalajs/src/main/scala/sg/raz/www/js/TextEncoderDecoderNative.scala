package sg.raz.www.js

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.TypedArrayBufferOps._
import scala.scalajs.js.typedarray._

@js.native
@JSGlobal
class TextDecoder(utfLabel: js.UndefOr[String] = js.undefined) extends js.Object {
  def decode(data: ArrayBufferView): String = js.native
}

@js.native
@JSGlobal
class TextEncoder(utfLabel: js.UndefOr[String] = js.undefined) extends js.Object {
  def encode(str: String): Uint8Array = js.native
}
