import AssemblyKeys._ // put this at the top of the file

name := "epic"

version := "0.1-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.scalanlp" %% "breeze-core" % "0.3-SNAPSHOT",
  "org.scalanlp" %% "breeze-math" % "0.3-SNAPSHOT",
  "org.scalanlp" %% "breeze-process" % "0.3-SNAPSHOT",
  "org.scalanlp" %% "breeze-learn" % "0.3-SNAPSHOT",
  "com.twitter"  %% "util-collection"  % "6.3.0"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.2" =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.4.RC2" % "test"))
    case x if x.startsWith("2.8") =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.3" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"))
    case _       =>
     (deps :+ ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test")
           :+ ("org.scalatest" %% "scalatest" % "2.0.M5b" % "test"))
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")


scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize")

javaOptions += "-Xmx2g"


seq(assemblySettings: _*)

