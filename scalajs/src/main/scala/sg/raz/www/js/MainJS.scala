package sg.raz.www.js

import org.scalajs.dom.{Event, HTMLDivElement, HTMLInputElement,HTMLButtonElement, Request, RequestInit, URLSearchParams, Chunk, document, window}
import window.fetch

import scala.collection.immutable.TreeMap
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

val mugshots = Array(
  "2023-09-16 11.44.08.png",
  "2023-09-16 11.50.10.png",
  "2023-09-16 12.32.48-1.png",
  "2023-09-16 12.32.51-1.png",
  "2023-09-16 12.43.46-1.png",
  "2023-09-16 12.43.46.png",
  "2023-09-16 12.43.47 (1).png",
  "2023-09-16 12.43.47-1.png",
  "2023-09-16 12.43.47.png",
  "2023-09-16 12.43.48-1.png",
  "2023-09-16 12.43.48.png",
  "2023-09-16 12.46.54-1.png",
  "2023-09-16 12.46.55.png",
  "2023-09-16 12.46.58-1.png",
  "2023-09-16 12.46.58.png",
  "2023-09-16 13.15.36-1.png",
  "2023-09-17 01.33.16-1.png",
  "2023-09-17 01.33.24.png",
  "2023-09-17 11.31.16.png",
  "2023-09-17 11.40.22.png",
  "2023-09-17 11.40.23.png",
  "2023-09-17 11.40.25-1.png",
  "2023-09-17 11.40.27.png",
  "2023-09-17 11.40.28.png",
  "2023-09-17 11.40.29-1.png",
  "2023-09-17 11.40.29.png",
  "2023-09-17 11.40.31.png",
  "2023-09-17 11.40.34.png",
  "2023-09-17 11.41.16 (1).png",
  "2023-09-17 11.41.17-1.png",
  "2023-09-17 11.41.19.png",
  "2023-09-17 11.41.21-1.png",
  "2023-09-17 11.41.22.png",
  "2023-09-17 11.41.26-1.png",
  "2023-09-17 11.48.40-2.png",
  "2023-09-17 11.48.41-1.png",
  "2023-09-17 11.48.48-1.png",
  "2023-09-17 11.52.42.png",
  "2023-09-17 11.52.44.png",
)

import org.scalajs.dom
import org.scalajs.dom.{window, document}
import org.scalajs.dom.{HTMLDivElement, Image, HttpMethod, Response, Request}

import scala.util.Random

@main
def main(): Unit = window.sessionStorage.getItem("whichHtml") match {
  case "index" => index()
  case "members" => members()
}

def index(): Unit = {
  window.addEventListener("resize", renderIndex _)
  window.addEventListener("load", renderIndex _)
}

def renderIndex(e: Event ): Unit =
  window.setTimeout( () => {
    loadMugshot()
    occupFontSize()
    showQR()
  }, 100)

def getFontSize(e: HTMLDivElement): Double =
  val fss = e.style.fontSize
  fss.substring(0, fss.length - 2).toDouble

def adjustFontSize(e: HTMLDivElement, initFntSz: Int): Unit =
  e.style.fontSize = s"${initFntSz}px"
  val (_, fw) = Iterator.iterate((e.scrollWidth, getFontSize(e) * 1.1)) { w =>
    val newFs = getFontSize(e) * 0.9
    e.style.fontSize = s"${newFs}px"
    (e.scrollWidth, newFs)
  }.dropWhile(_._1 >= window.innerWidth).next
  e.style.fontSize = (fw * 0.9).toInt + "px"

def occupFontSize(): Unit =
  val occup = document.getElementById("occupation").asInstanceOf[HTMLDivElement]
  adjustFontSize(occup, 35)
  occup.style.opacity = "1"

def newMugshot(): Image =
  val mugshot = Image()
  mugshot.height = 200
  mugshot.width = 200
  mugshot.style.display = "block"
  mugshot.style.marginLeft = "auto"
  mugshot.style.marginRight = "auto"
  mugshot.style.animation = "fadeIn 1s"
  mugshot.style.animationPlayState = "paused"
  mugshot.src = "static/mugshots/" + mugshots(Random.nextInt(mugshots.length))
  mugshot.id = "mugshot"
  mugshot.addEventListener("click", _ => loadMugshot())
  mugshot

def mugshotAnimation(mugshot: Image): Unit =
  mugshot.addEventListener("load", { _ =>
    val mugshotPlaceholder = document.getElementById("mugshot").asInstanceOf[Image]
    Option(mugshotPlaceholder).foreach(_.replaceWith(mugshot))
    window.setTimeout(() => mugshot.style.animationPlayState = "running", 100)
  })

def loadMugshot(): Unit =
  val mugshot = newMugshot()
  mugshotAnimation(mugshot)

def showQR(): Unit =
  val qr = document.getElementById("qr").asInstanceOf[Image]
  if window.innerHeight < 680 then
    qr.style.opacity = "0"
  else
    window.setTimeout(() => {
      qr.style.animationPlayState = "running"
      qr.style.opacity = "1"
    }, 100)

