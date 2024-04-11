package sg.raz.www

import com.typesafe.config.{Config, ConfigFactory}

import com.google.cloud.storage.Blob

import upickle.default.ReadWriter

object App extends cask.MainRoutes:
  override def host: String = "0.0.0.0"
  override def port: Int = sys.env.get("PORT").map(_.toInt).getOrElse(8080)

  case class User(name: String, passwd: String, token: String) derives ReadWriter
  case class Users(users: Array[User]) derives ReadWriter

  val userJson = scala.io.Source.fromResource("users.json").getLines().mkString
  val users = upickle.default.read[Users](userJson).users.map {
    case User(name, passwd, token) => (name -> (passwd, token))
  }.toMap
  
  @cask.postForm("/api/login")
  def login(User: String, Password: String): cask.Response[String] =
    val token: Option[String] = for
      u <- Option(User)
      ip <- Option(Password)
      sp <- users.get(u).map(_._1)
      if ip == sp
    yield users(u)._2
    token.map(cask.Response(_, 200, Nil, Nil)).getOrElse(cask.Response("", 401, Nil, Nil))

  @cask.postForm("/api/checktoken")
  def checkToken(User: String, AuthToken: String): cask.Response[String] =
    val check = for
      u <- Option(User)
      it <- Option(AuthToken)
      rt <- users.get(u).map(_._2)
      if it == rt
    yield 200
    cask.Response("", check.getOrElse(401), Nil, Nil)

  initialize()