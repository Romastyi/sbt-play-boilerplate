import sbt.Keys.crossSbtVersions
import xerial.sbt.Sonatype.sonatypeSettings

lazy val common = Seq(
  organization := "com.github.romastyi",
  version := "0.0.1-SNAPSHOT",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:_",
    "-unchecked",
    "-Xfatal-warnings"
  ),
  resolvers += Resolver.sonatypeRepo("releases")
) ++ sonatypePublish

lazy val sbtCommon = common ++ Seq(
  crossSbtVersions := List("0.13.16", "1.0.4"),
  scalaVersion := {
    CrossVersion partialVersion (sbtVersion in pluginCrossBuild).value match {
      case Some((0, 13)) => "2.10.6"
      case Some((1, _))  => "2.12.4"
      case _             => sys error s"Unhandled sbt version ${(sbtVersion in pluginCrossBuild).value}"
    }
  }
)

lazy val libsCommon = common ++ Seq(
  crossScalaVersions := List("2.11.12", "2.12.4")
)

lazy val lib = project
  .in(file("lib"))
  .settings(sbtCommon: _ *)
  .settings(
    name := """sbt-play-boilerplate-lib""",
    libraryDependencies ++= Seq(
      "com.eed3si9n" %% "treehugger" % "0.4.3",
      "io.swagger" % "swagger-parser" % "1.0.32",
      "org.scalatest" %% "scalatest" % "3.0.1" % "test"
    )
  )

lazy val plugin = project
  .in(file("plugin"))
  .settings(sbtCommon: _ *)
  .settings(
    name := """sbt-play-boilerplate""",
    sbtPlugin := true
  )
  .dependsOn(lib)

lazy val api = project
  .in(file("api"))
  .settings(libsCommon: _ *)
  .settings(
    name := """play-boilerplate-api""",
    libraryDependencies += "com.typesafe" % "config" % "1.3.2" % "provided"
  )

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true
  )
  .aggregate(lib, plugin, api)

publishArtifact := false

lazy val sonatypePublish = sonatypeSettings ++ Seq(
  publishMavenStyle := true,
  pomIncludeRepository := { _ =>
    false
  },
  credentials += Credentials(Path.userHome / ".sbt" / "nexus.credentials"),
  pomExtra := {
    <url>https://github.com/Romastyi/sbt-play-boilerplate</url>
      <licenses>
        <license>
          <name>Apache 2</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        </license>
      </licenses>
      <developers>
        <developer>
          <id>andreaTP</id>
          <name>Andrea Peruffo</name>
          <url>https://github.com/andreaTP/</url>
        </developer>
        <developer>
          <id>fralken</id>
          <name>Francesco Montecuccoli Degli Erri</name>
          <url>https://github.com/fralken/</url>
        </developer>
        <developer>
          <id>mfirry</id>
          <name>Marco Firrincieli</name>
          <url>https://github.com/mfirry/</url>
        </developer>
        <developer>
          <id>romastyi</id>
          <name>Romastyi</name>
          <url>https://github.com/Romastyi/</url>
        </developer>
      </developers>
  }
)
