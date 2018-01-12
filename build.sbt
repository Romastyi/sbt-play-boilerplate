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
       |  val organization = "%s"
       |  val current = "%s"
       |  val scalaVersion = "%s"
       |  val sbtVersion = "%s"
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
  version := "0.1.0-SNAPSHOT",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-language:_",
    "-unchecked",
    "-Xfatal-warnings"
  ),
  resolvers += sonatypeRepo(isSnapshot.value)
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

lazy val lib = project
  .in(file("lib"))
  .settings(common: _ *)
  .settings(
    name := """sbt-play-boilerplate-lib""",
    crossScalaVersions := List("2.10.6", "2.12.4"),
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

def apiProject(suffix: String, playVersion: String): Project = {
  Project(s"api-$suffix", file(s"api-$suffix"))
    .settings(common: _ *)
    .settings(
      name := s"""play-boilerplate-api-$suffix""",
      scalaVersion := "2.11.12",
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play" % playVersion % "provided",
        "com.typesafe.play" %% "play-ws" % playVersion % "provided"
      ),
      unmanagedSourceDirectories in Compile += {
        baseDirectory.value / ".." / "api" / "src" / "main" / "scala"
      }
    )
}

lazy val `api-play23` = apiProject("play23", "2.3.10")
lazy val `api-play24` = apiProject("play24", "2.4.11")
lazy val `api-play25` = apiProject("play25", "2.5.18")
lazy val `api-play26` = apiProject("play26", "2.6.7")
  .settings(crossScalaVersions := List("2.11.12", "2.12.4"))

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true
  )
  .aggregate(lib, plugin, `api-play23`, `api-play24`, `api-play25`, `api-play26`)

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
