import sbt.Keys.{crossSbtVersions, organization}
import xerial.sbt.Sonatype.sonatypeSettings

def sonatypeRepo(isSnapshot: Boolean): MavenRepository = {
  if (isSnapshot) Opts.resolver.sonatypeSnapshots
  else Opts.resolver.sonatypeStaging
}

def PluginVersion(organization: String,
                  version: String,
                  scalaVersion: String,
                  sbtVersion: String,
                  dir: File): Seq[File] = {
  val file = dir / "PluginVersion.scala"
  val scalaSource =
    """|package play.boilerplate.core
       |
       |object PluginVersion {
       |  val organization: String = "%s"
       |  val current: String = "%s"
       |  val scalaVersion: String = "%s"
       |  val sbtVersion: String = "%s"
       |}
       |""".stripMargin
      .format(
        organization,
        version,
        scalaVersion,
        sbtVersion
      )

  if (!file.exists() || IO.read(file) != scalaSource) {
    IO.write(file, scalaSource)
  }

  Seq(file)
}

lazy val common = Seq(
  organization := "com.github.romastyi",
  version := "0.3.0-SNAPSHOT",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:_",
    "-unchecked",
    "-Xfatal-warnings"
  ),
  resolvers += sonatypeRepo(isSnapshot.value),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
) ++ sonatypePublish

// SBT plugin

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

lazy val lib = project
  .in(file("sbt-plugin/lib"))
  .settings(common: _ *)
  .settings(
    name := """sbt-play-boilerplate-lib""",
    crossScalaVersions := List("2.10.6", "2.12.4"),
    libraryDependencies ++= Seq(
      "com.eed3si9n" %% "treehugger" % "0.4.3",
      "io.swagger" % "swagger-parser" % "1.0.41"
    )
  )

lazy val plugin = project
  .in(file("sbt-plugin/plugin"))
  .settings(sbtCommon: _ *)
  .settings(
    name := """sbt-play-boilerplate-plugin""",
    sbtPlugin := true,
    sourceGenerators in Compile += Def.task(PluginVersion(
      organization.value,
      version.value,
      scalaVersion.value,
      (sbtVersion in pluginCrossBuild).value,
      (sourceManaged in Compile).value)
    ).taskValue
  )
  .dependsOn(lib)

// Client API libraries

lazy val `api-client-core` = Project("api-client-core", file("api-client/core"))
  .settings(common: _ *)
  .settings(
    name := """play-boilerplate-api-client-core""",
    scalaVersion := "2.11.12",
    crossScalaVersions := List("2.11.12", "2.12.4"),
    libraryDependencies += "com.typesafe" % "config" % "1.3.1"
  )

lazy val `api-client-consul` = Project("api-client-consul", file("api-client/consul"))
  .settings(common: _ *)
  .settings(
    name := """play-boilerplate-api-client-consul""",
    scalaVersion := "2.11.12",
    crossScalaVersions := List("2.11.12", "2.12.4"),
    libraryDependencies += "com.ecwid.consul" % "consul-api" % "1.2.4"
  )
  .dependsOn(`api-client-core`)

def clientApiProject(suffix: String): Project = {
  Project(s"api-client-$suffix", file(s"api-client/$suffix"))
    .settings(common: _ *)
    .settings(
      name := s"""play-boilerplate-api-client-$suffix""",
      scalaVersion := "2.11.12",
      unmanagedSourceDirectories in Compile += {
        baseDirectory.value / ".." / "share" / "src" / "main" / "scala"
      },
      unmanagedSourceDirectories in Test += {
        baseDirectory.value / ".." / "share" / "src" / "test" / "scala"
      }
    )
    .dependsOn(`api-client-core`)
}

lazy val `api-client-play24` = clientApiProject("play24")
  .settings(libraryDependencies += "com.typesafe.play" %% "play-ws" % "2.4.11" % "provided")
lazy val `api-client-play25` = clientApiProject("play25")
  .settings(libraryDependencies += "com.typesafe.play" %% "play-ws" % "2.5.18" % "provided")
lazy val `api-client-play26` = clientApiProject("play26")
  .settings(
    crossScalaVersions := List("2.11.12", "2.12.4"),
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-ws-standalone" % "1.1.12",
      "com.typesafe.play" %% "play-ws-standalone-json" % "1.1.12"
    )
  )
lazy val `api-client-play27` = clientApiProject("play27")
  .settings(
    crossScalaVersions := List("2.11.12", "2.12.8"),
    libraryDependencies ++= Seq(
      "com.typesafe.play" %% "play-ws-standalone" % "2.0.1",
      "com.typesafe.play" %% "play-ws-standalone-json" % "2.0.1"
    )
  )

// Server API libraries

def serverApiProject(suffix: String, playVersion: String): Project = {
  Project(s"api-server-$suffix", file(s"api-server/$suffix"))
    .settings(common: _ *)
    .settings(
      name := s"""play-boilerplate-api-server-$suffix""",
      scalaVersion := "2.11.12",
      libraryDependencies += "com.typesafe.play" %% "play" % playVersion % "provided",
      unmanagedSourceDirectories in Compile += {
        baseDirectory.value / ".." / "share" / "src" / "main" / "scala"
      }
    )
}

lazy val `api-server-play24` = serverApiProject("play24", "2.4.11")
  .dependsOn(`api-client-play24`)
lazy val `api-server-play25` = serverApiProject("play25", "2.5.18")
  .dependsOn(`api-client-play25`)
lazy val `api-server-play26` = serverApiProject("play26", "2.6.21" )
  .settings(crossScalaVersions := List("2.11.12", "2.12.4"))
  .dependsOn(`api-client-play26`)
lazy val `api-server-play27` = serverApiProject("play27", "2.7.0" )
  .settings(crossScalaVersions := List("2.11.12", "2.12.8"))
  .dependsOn(`api-client-play27`)

def scaldiPlayProject(suffix: String, playVersion: String): Project = {
  Project(s"api-scaldi-$suffix", file(s"api-scaldi/scaldi-$suffix"))
    .settings(common: _ *)
    .settings(
      name := s"""play-boilerplate-scaldi-$suffix""",
      scalaVersion := "2.11.12",
      libraryDependencies += "com.typesafe.play" %% "play" % playVersion % "provided",
      libraryDependencies += (CrossVersion partialVersion playVersion match {
        case Some((2, 4)) =>
          "org.scaldi" %% "scaldi-play" % "0.5.13"
        case Some((2, 5)) =>
          "org.scaldi" %% "scaldi-play" % "0.5.15"
        case Some((2, 6)) =>
          "org.scaldi" %% "scaldi-play" % "0.5.17"
        case Some((2, 7)) =>
          // FIXME: This is version for Play 2.6.x
          "org.scaldi" %% "scaldi-play" % "0.5.17"
        case _ =>
          "org.scaldi" %% "scaldi-play" % "0.5.17"
      }),
      unmanagedSourceDirectories in Compile += {
        baseDirectory.value / ".." / "share" / "src" / "main" / "scala"
      },
      unmanagedResourceDirectories in Compile += {
        baseDirectory.value / ".." / "share" / "src" / "main" / "resources"
      }
    )
}

lazy val `api-scaldi-play24` = scaldiPlayProject("play24", "2.4.11")
lazy val `api-scaldi-play25` = scaldiPlayProject("play25", "2.5.18")
lazy val `api-scaldi-play26` = scaldiPlayProject("play26", "2.6.21" )
  .settings(crossScalaVersions := List("2.11.12", "2.12.4"))
lazy val `api-scaldi-play27` = scaldiPlayProject("play27", "2.7.0" )
  .settings(crossScalaVersions := List("2.11.12", "2.12.8"))

// ---

lazy val root = Project("sbt-play-boilerplate", file("."))
  .settings(
    skip in publish := true
  )
  .aggregate(
    lib, plugin,
    `api-client-core`, `api-client-consul`,
    `api-client-play24`, `api-client-play25`, `api-client-play26`, `api-client-play27`,
    `api-server-play24`, `api-server-play25`, `api-server-play26`, `api-server-play27`,
    `api-scaldi-play24`, `api-scaldi-play25`, `api-scaldi-play26`, `api-scaldi-play27`
  )

publishArtifact := false

lazy val sonatypePublish = sonatypeSettings ++ Seq(
  publishMavenStyle := true,
  publishTo := Some(sonatypeRepo(isSnapshot.value)),
  pomIncludeRepository := { _ =>
    false
  },
  credentials += Credentials(Path.userHome / ".sbt" / "sonatype.credentials"),
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
      <scm>
        <url>https://github.com/romastyi/sbt-play-boilerplate/tree/master</url>
        <connection>scm:git:git://github.com/romastyi/sbt-play-boilerplate.git</connection>
        <developerConnection>scm:git:git://github.com/romastyi/sbt-play-boilerplate.git</developerConnection>
      </scm>
  }
)
