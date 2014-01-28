import sbt._
import sbt.Keys._

object Version {
  val scala = "2.10.3"
  val scalaTest = "1.9.1"
  val spray = "1.2.0"
  val akka = "2.2.3"
}

object Deps {
  val testBasics = Seq(
    "org.scalatest" %% "scalatest" % "1.9.1",
    "org.scalacheck" %% "scalacheck" % "1.10.1",
    "io.spray" % "spray-can" % Version.spray,
    "io.spray" % "spray-testkit" % Version.spray
  ) map (_ % "test")

  val akka = Seq(
    "com.typesafe.akka" %% "akka-actor" % Version.akka % "provided",
    "com.typesafe.akka" %% "akka-testkit" % Version.akka % "test"
  )

  val logback = Seq(
    "org.slf4j" % "slf4j-api" % "1.7.5",
    "ch.qos.logback" % "logback-classic" % "1.0.13" % "runtime",
    "com.typesafe.akka" %% "akka-slf4j" % "2.2.3" % "runtime"
  )

  val spray = Seq(
    "io.spray" % "spray-client" % Version.spray,
    "io.spray" % "spray-routing" % Version.spray
  )
}

object SprayOpenId extends sbt.Build {
  import sbtbuildinfo.Plugin._
  
  lazy val module = Project(
    id = "spray-openid",
    base = file("."),
    settings = Project.defaultSettings ++ buildInfoSettings ++ Seq(
      name := "spray-openid",
      sourceGenerators in Compile <+= buildInfo,
      buildInfoKeys := Seq(name, version, scalaVersion, buildTimeKey, gitRevKey),
      buildInfoPackage := "org.eknet.spray.openid",
      libraryDependencies ++= Deps.spray ++ Deps.akka ++ Deps.testBasics
    )
  )

  lazy val buildTimeKey = BuildInfoKey.action("buildTime") {
    System.currentTimeMillis
  }
  lazy val gitRevKey = BuildInfoKey.action("revision") {
    Process("git rev-parse HEAD").lines.head
  }
  
  override lazy val settings = super.settings ++ Seq(
    version := "0.1.0-SNAPSHOT",
    resolvers ++= Seq("spray repo" at "http://repo.spray.io"),
    publishTo := Some("eknet-maven2" at "https://eknet.org/maven2"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    pomIncludeRepository := { _ => false },
    organization := "org.eknet.porter",
    scalaVersion := Version.scala,
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    licenses := Seq("ASL2" -> url("http://www.apache.org/LICENESE-2.0.txt"))
  )
}
