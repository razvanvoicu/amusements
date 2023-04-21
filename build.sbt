ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "www",
    assembly / assemblyJarName := "www-" + version.value + ".jar",
    //idePackagePrefix := Some("sg.raz.www"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.0",
      "com.typesafe" % "config" % "1.4.2",
      "com.google.cloud" % "google-cloud-storage" % "2.22.0"
    )
  )

lazy val scalajs = (project in file("scalajs"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scalajs",
    //idePackagePrefix := Some("sg.raz.www.js"),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(

    )
  )

lazy val prepareDeploy = taskKey[Unit]("Prepares package for GCP app deploy")

assembly / assemblyMergeStrategy := {
  case PathList("META-INF","versions","9","module-info.class") =>
    println("=====")
    MergeStrategy.discard
//  case p @ PathList("com", "google", "code", "qson", xs @ _*) =>
//    println(("==========", p))
//    MergeStrategy.first
//  case p@PathList("com", "fasterxml", "jackson", "core", xs@_*) =>
//    println(("==========", p))
//    MergeStrategy.last
  case x @ PathList(y, xs @ _*) =>
    //println(("!!!", y, xs, x))
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

prepareDeploy := {
  import scala.sys.process._
  val javaProcs =
    """tasklist /fi "imagename eq java.exe""""
      .!!
      .split("\n")
      .drop(3)
      .map(_.split("[ \t]+")(1).toInt)
  println(s"Found java processes: ${javaProcs.toList}")

  val netStatProcNo =
    """netstat -a -n -p tcp -o"""
      .!!
      .split("\n")
      .map(_.split("[ \t]+"))
      .find(_.exists(_.trim == "0.0.0.0:8080")).toList
      .map(_(5))
      .map(_.filter(_.isDigit).toInt)

  println(s"Process listening on 8080:  ${netStatProcNo}")

  val killCmd = s"""taskkill /F /PID ${netStatProcNo.headOption.getOrElse(0)}"""

  if (netStatProcNo.nonEmpty && (javaProcs contains netStatProcNo.head)) {
    println(s"Issuing kill command: $killCmd")
    killCmd.!!
  }

  println("Waiting for regular build...")
  val jarFile = (root / assembly).value
  val _ = (scalajs / Compile / fastLinkJS).value

  println(s"Copying assembly ${jarFile} to deployment folder")
  sbt.io.IO.copyFile(jarFile, new File(s"./deploy/${jarFile.name}"))

  println(s"Copying main JS file to deployment folder")
  sbt.io.IO.copyFile(new File("scalajs/target/scala-3.1.3/scalajs-fastopt/main.js"), new File("./deploy/main.js"))
  sbt.io.IO.copyFile(new File("scalajs/target/scala-3.1.3/scalajs-fastopt/main.js.map"), new File("./deploy/main.js.map"))

  val localBackendStart = s"""java -D"app.env"="debug" -jar .\\deploy\\${jarFile.name}"""
  println(s"Starting local backend for testing: ${localBackendStart}")
  localBackendStart.run()

  println("You may now test and eventually deploy from the /deploy/ folder")
}