name := "chalk"

version := "0.1-SNAPSHOT"

organization := "opennlp"

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
