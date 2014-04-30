import AssemblyKeys._ // put this at the top of the file

name := "epic"

version := "0.1-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.10.3"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.scalanlp" %% "breeze" % "0.8-SNAPSHOT",
  "org.scalanlp" %% "breeze-config" % "0.8-SNAPSHOT",
  "org.scalanlp" % "chalk" % "1.3.0" intransitive(),
  "org.scalanlp" % "nak" % "1.2.0" intransitive(),
  //"org.scalanlp" %% "breeze-process" % "0.3-SNAPSHOT",
  //"org.scalanlp" %% "breeze-learn" % "0.3-SNAPSHOT",
  "org.mapdb" % "mapdb" % "0.9.2",
  "org.slf4j" % "slf4j-simple" % "1.7.6",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)


credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")


scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx2g"


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
