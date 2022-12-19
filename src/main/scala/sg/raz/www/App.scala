package sg.raz.www

import com.typesafe.config.{Config, ConfigFactory}

import java.io.InputStreamReader
import java.sql.{Connection, DriverManager}

object App extends cask.MainRoutes:
  override def host: String = "0.0.0.0"
  override def port: Int = sys.env.get("PORT").map(_.toInt).getOrElse(8080)

  @cask.get("/hello")
    def hello() : String =
      val s = DbDetails.conn.createStatement()
      val rs = s.executeQuery("select name from users where id=1")
      rs.next()
      rs.getString("name") * 3

  @cask.post("/do-thing")
  def doThing(request: cask.Request): String = request.text().reverse

  initialize()