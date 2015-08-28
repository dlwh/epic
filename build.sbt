import AssemblyKeys._
import sbt.Keys._
import Version._
import com.typesafe.sbt.osgi.SbtOsgi._

lazy val extra = <url>http://scalanlp.org/</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:dlwh/epic.git</url>
    <connection>scm:git:git@github.com:dlwh/epic.git</connection>
  </scm>
  <developers>
    <developer>
      <id>dlwh</id>
      <name>David Hall</name>
      <url>http://cs.berkeley.edu/~dlwh/</url>
    </developer>
  </developers>;

lazy val commonSettings = Seq(
  organization := "org.scalanlp",
  version := "0.4-SNAPSHOT",
  scalaVersion := Version.scala,
  crossScalaVersions := Seq("2.11.7", "2.10.4"),
  libraryDependencies ++= Seq(
    Library.breeze,
    Library.breezeConfig,
    Library.mapdb,
    Library.scalaLoggingSlf4j,
    Library.tikaParsers % "compile,optional",
    Library.boilerpipe,
    Library.nekohtml,
    Library.slf4jSimple,
    Library.commonsLang3,
    Library.jflex,
    Library.scalatest % "test",
    Library.scalacheck % "test",
    Library.junit % "test"
  ),
  scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize"),
  javaOptions += "-Xmx4g",
  javaOptions += "-Xrunhprof:cpu=samples,depth=12",
  fork := true,
  publishMavenStyle := true,
  pomExtra := extra,
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  assemblyOption in assembly ~= {
    _.copy(cacheOutput = false)
  },
  resolvers ++= Seq(
    "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
    "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, add dependency on scala-xml module
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1")
      case _ =>
      Seq.empty
      })

  ) ++ assemblySettings ++ sbtjflex.SbtJFlexPlugin.jflexSettings ++ net.virtualvoid.sbt.graph.Plugin.graphSettings ++
  IndexedSeq (
      mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
        {
          case PathList("org", "w3c", "dom", _) => MergeStrategy.first
          case PathList("javax", "xml", "stream", _ *) => MergeStrategy.first
          case PathList("scala", "xml", _ *) => MergeStrategy.first
          case PathList("org", "cyberneko", "html", _ *) => MergeStrategy.first
          case x => old(x)
         }
      }
)




lazy val core = project
  .in(file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "epic"
    //,
    //OsgiKeys.privatePackage := Seq(),
    //OsgiKeys.importPackage := Seq("org.apache.tika.*; resolution:=optional", "*"),
    //OsgiKeys.exportPackage := Seq("epic.*")
  )
