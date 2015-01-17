name := "epic-slab"

version := "0.2-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.11.4"

crossScalaVersions  := Seq("2.11.4", "2.10.4")

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

val shapeless = Def setting (
    CrossVersion partialVersion scalaVersion.value match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 => 
      "com.chuusai" %% "shapeless" % "2.0.0"
    case Some((2, 10)) => 
      "com.chuusai" %  "shapeless" % "2.0.0" cross CrossVersion.full
  }
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  shapeless.value,
  "org.scalaz" %% "scalaz-core" % "7.1.0"
)

scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx4g"

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
    <url>git@github.com:reactormonk/slabs.git</url>
    <connection>scm:git:git@github.com:reactormonk/slabs.git</connection>
  </scm>
  <developers>
    <developer>
      <id>reactormonk</id>
      <name>Simon Hafner</name>
      <url>http://reactormonk.org</url>
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
