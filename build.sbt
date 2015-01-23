name := "epic-opennlp"

version := "0.0.1-SNAPSHOT"

organization := "org.reactormonk"

scalaVersion := "2.11.4"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.scalanlp" %% "epic-slab" % "0.2-SNAPSHOT",
  "org.apache.spark" %% "spark-core" % "1.2.0" % "provided",
  "org.apache.opennlp" % "opennlp" % "1.5.3",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test",
  "org.apache.opennlp" % "opennlp-tools" % "1.5.3"
)

javaOptions += "-Xmx4g"

mainClass in assembly := Some("org.reactormonk.epic.opennlp.SparkPipeline")
