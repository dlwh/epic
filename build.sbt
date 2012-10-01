import AssemblyKeys._ // put this at the top of the file
import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "epic"

version := "0.1-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.9.2"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-process" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-learn" % "0.2-SNAPSHOT"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.2" =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.4.RC2" % "test"))
    case x if x.startsWith("2.8") =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.3" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"))
    case x  => error("Unsupported Scala version " + x)
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

scalacOptions ++= Seq("-deprecation","-target:jvm-1.5")

javaOptions += "-Xmx2g"


seq(assemblySettings: _*)


seq(jacoco.settings : _*)
