import AssemblyKeys._
import sbtassembly.AssemblyOption

lazy val commonSettings = Seq(
  scalaVersion := "2.11.4",
  organization := "org.scalanlp",
  crossScalaVersions  := Seq("2.11.4", "2.10.4"),
  resolvers ++= Seq(
    "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
    "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize"),
  javaOptions += "-Xmx4g",
  javaOptions += "-Xrunhprof:cpu=samples,depth=12",
  publishMavenStyle := true,
  fork := true,
  pomExtra := (
    <url>http://scalanlp.org/</url>
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
      <developer>
        <id>reactormonk</id>
        <name>Simon Hafner</name>
        <url>http://reactormonk.org</url>
      </developer>
    </developers>),
  publishTo <<= version { (v: String) =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT")) 
      Some("snapshots" at nexus + "content/repositories/snapshots") 
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false }
)

lazy val root = (project in file("."))
  .settings(commonSettings: _*)
  .settings(name := "epic")
  .settings(version := "0.3-SNAPSHOT")
  .settings(libraryDependencies ++= Seq(
    "junit" % "junit" % "4.5" % "test",
    "org.scalanlp" %% "breeze" % "0.10",
    "org.scalanlp" %% "breeze-config" % "0.9.1",
    "org.scalanlp" %% "nak" % "1.3",
    "org.scalanlp" %% "epic-slab" % "0.3-SNAPSHOT",
    "org.mapdb" % "mapdb" % "0.9.2",
    ("org.apache.tika" % "tika-parsers" % "1.5").exclude ("edu.ucar", "netcdf").exclude("com.googlecode.mp4parser","isoparser"),
    "de.l3s.boilerpipe" % "boilerpipe" % "1.1.0",
    "net.sourceforge.nekohtml" % "nekohtml" % "1.9.21",//needed by boilerpipe
    "org.slf4j" % "slf4j-simple" % "1.7.6",
    "org.apache.commons" % "commons-lang3" % "3.3.2",
    "de.jflex" % "jflex" % "1.6.0" % "compile",
    "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    "org.typelevel" %% "scodec-core" % "1.1.0"
  ))
  .settings(libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    // if scala 2.11+ is used, add dependency on scala-xml module
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1")
    case _ =>
      Seq.empty
  }))
  .dependsOn(slab)
  .settings(test in Test <<= test in Test dependsOn (test in Test in slab))

seq(assemblySettings: _*)

assemblyOption in assembly ~= { _.copy(cacheOutput = false) }

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("org", "w3c", "dom", _) => MergeStrategy.first
    case PathList("javax", "xml", "stream", _ *) => MergeStrategy.first
    case PathList("org", "cyberneko", "html", _ *) => MergeStrategy.first
    case x => old(x)
  }
}

seq(sbtjflex.SbtJFlexPlugin.jflexSettings: _*)

net.virtualvoid.sbt.graph.Plugin.graphSettings

val shapeless = Def setting (
    CrossVersion partialVersion scalaVersion.value match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 => 
      "com.chuusai" %% "shapeless" % "2.0.0"
    case Some((2, 10)) => 
      "com.chuusai" %  "shapeless" % "2.0.0" cross CrossVersion.full
  }
)

lazy val slab = (project in file("slab"))
  .settings(commonSettings: _*)
  .settings(name := "epic-slab")
  .settings(version := "0.3-SNAPSHOT")
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    shapeless.value,
    "org.scalaz" %% "scalaz-core" % "7.1.0"
  ))
