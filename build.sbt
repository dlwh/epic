name := "chalk"

version := "0.1-SNAPSHOT"

organization := "com.jasonbaldridge"

scalaVersion := "2.9.2"

crossPaths := false

resolvers ++= Seq(
  "opennlp sourceforge repo" at "http://opennlp.sourceforge.net/maven2"
)

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.8" % "test->default",
  "jwnl" % "jwnl" % "1.3.3" % "compile",
  "org.osgi" % "org.osgi.core" % "4.2.0" % "provided",
  "org.osgi" % "org.osgi.compendium" % "4.2.0" % "provided",
  "org.apache.uima" % "uimaj-core" % "2.3.1" % "provided"
)

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/jasonbaldridge/chalk</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:jasonbaldridge/chalk.git</url>
    <connection>scm:git:git@github.com:jasonbaldridge/chalk.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jasonbaldridge</id>
      <name>Jason Baldridge</name>
      <url>http://www.jasonbaldridge.com</url>
    </developer>
  </developers>
)