package sg.raz.www.js

import org.scalajs.dom.{Chunk, Event, HTMLButtonElement, HTMLDivElement, HTMLInputElement, HttpMethod, ReadableStreamReader, Request, RequestInit, Response, URLSearchParams, document, fetch, window}
import sg.raz.www.js.MorphMethod.{Morph, Contour}

import scala.collection.immutable.TreeMap
import scala.scalajs.js
import scala.scalajs.js.{Dictionary, Promise}
import scala.scalajs.js.typedarray.Uint8Array

val apps = TreeMap[String, (Event, HTMLDivElement) => Unit] (
  "Lissajous" -> lissajous,
  "Wheely" -> wheely,
  "Shapes" -> shapes,
  "Morph" -> morph(Morph),
  "Contour" -> morph(Contour),
  )

def members(): Unit =
  setMembersEventHandlers()
  setupLogoutHandler()
  tryRetrieveCredsAndDecideExecutionContext()

def setMembersEventHandlers(): Unit =
  window.addEventListener("load", renderMembers)
  window.addEventListener("resize", renderMembers)

val renderMembers: Event => Unit = _ => {
  val form = document.getElementById("loginform").asInstanceOf[HTMLDivElement]
  form.style.left = s"${((window.innerWidth - 220) / 2)}px"
}

def storeCreds(user: String, authToken: String) =
  window.localStorage.setItem("user", user)
  window.localStorage.setItem("authToken", authToken)

def clearCreds(): Unit = Seq("user", "authToken").foreach(window.localStorage.removeItem(_))

def setupLogoutHandler(): Unit = document.getElementById("logout").addEventListener("click", _ => clearCreds())

def tryRetrieveCredsAndDecideExecutionContext(): Unit =
  val userName = Option(window.localStorage.getItem("user")).filter(_.nonEmpty)
  val authToken = Option(window.localStorage.getItem("authToken")).filter(_.nonEmpty)
  userName.flatMap(u => authToken.map(t => (u, t))) match {
    case Some((u, t)) => memberLoggedIn(u, t)
    case None => memberMustLogIn()
  }

def memberLoggedIn(userName: String, authToken: String): Unit = {
  verifyToken(userName, authToken) `then` authenticateLoginAndProceedToApps(userName, authToken)
}

def verifyToken(userName: String, authToken: String): Promise[Response] =
  fetch(new Request("/api/checktoken", new RequestInit {
    method = HttpMethod.POST
    body = URLSearchParams(js.Dictionary( "User" -> userName, "AuthToken" -> authToken))
  }))

def authenticateLoginAndProceedToApps(userName: String, authToken: String): Response => Unit = r =>
  if r.status == 200
  then appsMenu(userName, authToken)
  else memberMustLogIn()

def readResponseBody(r: ReadableStreamReader[Uint8Array], prev: String): Promise[String] = {
  val td = new TextDecoder()
  r.read() `then` { (chunk: Chunk[Uint8Array]) =>
    if chunk.done then Promise.resolve(prev) else readResponseBody(r, prev + td.decode(chunk.value))
  }
}

def memberMustLogIn(): Unit = {
  clearCreds()
  showLoginForm()
  document.getElementById("submitBtn").addEventListener("click", (_: Event) => {
    val user = document.getElementById("uname").asInstanceOf[HTMLInputElement].value
    val passwd = document.getElementById("psw").asInstanceOf[HTMLInputElement].value
    authenticate(user, passwd) `then` appMenuOrError
  })
}

def authenticate(user: String, passwd: String): Promise[Response] =
  fetch(new Request("/api/login", new RequestInit {
    method = HttpMethod.POST
    body = URLSearchParams(Dictionary("User" -> user, "Password" -> passwd))
  }))

def showLoginForm(): Unit = window.setTimeout(() => {
  val lf = document.getElementById("loginform").asInstanceOf[HTMLDivElement]
  lf.removeAttribute("hidden")
  lf.style.visibility = "visible"
}, 100)

def appsMenu(user: String, authToken: String): Unit =
  storeCreds(user, authToken)
  val apparea = document.getElementById("apparea").asInstanceOf[HTMLDivElement]
  setAppAreaRszEvHnd(apparea)
  val ap = makeAppPanel()
  window.addEventListener("resize", _ => {ap.style.width = s"${window.innerWidth - 20}px"})
  makeAppButtons(ap, apparea)
  makeVisible(ap)

def setAppAreaRszEvHnd(appArea: HTMLDivElement): Unit =
  resizeAppArea(appArea)
  window.addEventListener("resize", resizeAppArea(appArea))
  window.addEventListener("load", resizeAppArea(appArea))

def makeVisible(e: HTMLDivElement): Unit =
  e.removeAttribute("hidden")
  e.style.visibility = "visible"

def makeHidden(e: HTMLDivElement): Unit =
  e.setAttribute("hidden", "hidden")
  e.style.visibility = "hidden"

def makePressed(b: HTMLButtonElement): Unit =
  b.style.color = "yellow"
  b.style.background = "steelblue"

def setAppButtonEvHnd(b: HTMLButtonElement, apppanel: HTMLDivElement, apparea: HTMLDivElement, app: String): Unit =
  b.addEventListener("click", ev => {
    makePressed(b)
    window.setTimeout(() => {
      makeHidden(apppanel)
      makeVisible(apparea)
      apps(app)(ev, apparea)
    }, 10)
  })

def setAppButtonStyle(appButton: HTMLButtonElement, app: String): Unit =
  appButton.id = app
  appButton.style.color = "orange"
  appButton.style.background = "navy"
  appButton.innerHTML = app
  appButton.style.display = "block"
  appButton.style.fontSize = "20px"
  appButton.style.margin = "10px auto"
  appButton.style.width = "120px"

def makeAppButton(app: String, apppanel: HTMLDivElement, apparea: HTMLDivElement): HTMLButtonElement =
  val appButton = document.createElement("button").asInstanceOf[HTMLButtonElement]
  setAppButtonStyle(appButton, app)
  setAppButtonEvHnd(appButton, apppanel, apparea, app)
  appButton

def makeAppButtons(apppanel: HTMLDivElement, apparea: HTMLDivElement): Unit = apps.keys.foreach { app =>
    val appButton = makeAppButton(app, apppanel, apparea)
    apppanel.appendChild(appButton)
  }

def makeAppPanel(): HTMLDivElement =
  val appPanel = document.getElementById("apppanel").asInstanceOf[HTMLDivElement]
  appPanel.innerHTML = ""
  appPanel.style.width = s"${window.innerWidth - 10}px"
  appPanel.style.textJustify = "center"
  appPanel.style.textAlign = "center"
  appPanel

def resizeAppArea(apparea: HTMLDivElement): Event => Unit = _ => {
  apparea.style.width = s"${window.innerWidth - 10}px"
  apparea.style.height = s"${window.innerHeight - 35}px"
}

val appMenuOrError: Response => Unit = resp =>
  val r = resp.body.getReader()
  if resp.status == 200 then
    readResponseBody(r, "") `then` displayAppsMenu

val displayAppsMenu: String => Unit = token =>
  storeCreds(document.getElementById("uname").asInstanceOf[HTMLInputElement].value, token)
  makeHidden(document.getElementById("loginform").asInstanceOf[HTMLDivElement])
  makeVisible(document.getElementById("apppanel").asInstanceOf[HTMLDivElement])
  appsMenu(document.getElementById("uname").asInstanceOf[HTMLInputElement].value, token)

