import AssemblyKeys._ // put this at the top of the file

name := "epic"

version := "0.2-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.11.1"

crossScalaVersions  := Seq("2.11.1", "2.10.4")

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.scalanlp" %% "breeze" % "0.8.1",
  "org.scalanlp" %% "breeze-config" % "0.8.1-SNAPSHOT",
  "org.scalanlp" %% "nak" % "1.3-SNAPSHOT",
  "org.mapdb" % "mapdb" % "0.9.2",
  "org.slf4j" % "slf4j-simple" % "1.7.6",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "com.chuusai" %% "shapeless" % "2.0.0",
  "org.typelevel" %% "scodec-core" % "1.1.0"
)

libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  // if scala 2.11+ is used, add dependency on scala-xml module
  case Some((2, scalaMajor)) if scalaMajor >= 11 =>
    Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.1")
    case _ =>
    Seq.empty
})



scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx4g"

//javaOptions += "-Xrunhprof:cpu=samples,depth=12"
//fork := true
seq(assemblySettings: _*)

publishMavenStyle := true



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
  </developers>)


  
publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}


publishArtifact in Test := false

pomIncludeRepository := { _ => false }

/*
excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
 cp filter {_.data.getName.matches(".native.")}
}
*/
assemblyOption in assembly ~= { _.copy(cacheOutput = false) }



excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
 cp filter {x => x.data.getName.matches("sbt.*") || x.data.getName.matches(".*macros.*")}
}

seq(sbtjflex.SbtJFlexPlugin.jflexSettings: _*)

net.virtualvoid.sbt.graph.Plugin.graphSettings
