import sbt._

object Plugins extends Build {
  lazy val root = Project("root", file(".")) dependsOn(
    uri("git://github.com/eed3si9n/sbt-assembly.git#master") // where XX is branch
  )
}


