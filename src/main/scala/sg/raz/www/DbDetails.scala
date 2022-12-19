package sg.raz.www

import com.typesafe.config.{Config, ConfigFactory}

import java.io.InputStreamReader
import java.sql.{Connection, DriverManager}

object DbDetails:
  Class.forName("org.postgresql.Driver")

  private val cfg: Config = ConfigFactory.parseReader(new InputStreamReader(getClass.getClassLoader.getResourceAsStream("secret.conf")))
  private val dbpwd = cfg.getString("db.passwd")
  private val dbname = cfg.getString("db.name")
  private val dbUrlCfg = "db" + (if System.getProperty("app.env") == "debug" then "debug" else "prod")
  private val dbUrl = cfg.getString(dbUrlCfg)
  val conn: Connection = DriverManager.getConnection(dbUrl, dbname, dbpwd)