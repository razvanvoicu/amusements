package sg.raz.www

import com.google.auth.Credentials
import com.google.auth.oauth2.GoogleCredentials
import com.typesafe.config.{Config, ConfigFactory}
import com.google.auth.oauth2.AccessToken

import java.io.{FileInputStream, InputStreamReader}
import com.google.cloud.storage.*
import com.google.cloud.storage.Blob.BlobSourceOption

import java.nio.charset.StandardCharsets

object App extends cask.MainRoutes:
  override def host: String = "0.0.0.0"
  override def port: Int = sys.env.get("PORT").map(_.toInt).getOrElse(8080)

  @cask.get("/hello")
    def hello() : String =
      val creds = GoogleCredentials.fromStream(ClassLoader.getSystemResourceAsStream("personalexperiments01-cebd4e9f1c35.json"))
      val storage: Storage = StorageOptions
        .newBuilder
        .setCredentials(creds)
        .setProjectId("personalexperiments01")
        .build
        .getService
      val blob: Blob = storage.get(BlobId.of("personalexperiments01.appspot.com", "bucket/test.json"))
      "Blob content: " + new String(blob.getContent(BlobSourceOption.generationMatch()), StandardCharsets.UTF_8)

  @cask.post("/do-thing")
  def doThing(request: cask.Request): String = request.text().reverse

  initialize()