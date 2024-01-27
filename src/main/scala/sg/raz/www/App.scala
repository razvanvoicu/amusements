package sg.raz.www

import com.google.auth.Credentials
import com.google.auth.oauth2.GoogleCredentials
import com.typesafe.config.{Config, ConfigFactory}
import com.google.auth.oauth2.AccessToken

import java.io.{FileInputStream, InputStreamReader}
import com.google.cloud.storage.{Blob, BlobId, Storage, StorageOptions}
import com.google.cloud.storage.Blob.BlobSourceOption

import java.nio.charset.StandardCharsets
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

//  @cask.get("/hello")
//    def hello() : String =
//      val creds = GoogleCredentials.fromStream(ClassLoader.getSystemResourceAsStream("personalexperiments01-cebd4e9f1c35.json"))
//      val storage: Storage = StorageOptions
//        .newBuilder
//        .setCredentials(creds)
//        .setProjectId("personalexperiments01")
//        .build
//        .getService
//      val blob: Blob = storage.get(BlobId.of("personalexperiments01.appspot.com", "bucket/test.json"))
//      "Blob content: " + new String(blob.getContent(BlobSourceOption.generationMatch()), StandardCharsets.UTF_8)
//
//  @cask.post("/debug")
//  def doThing(request: cask.Request): String = userJson

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