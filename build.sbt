import sbt.Keys._
import Version._
// import com.typesafe.sbt.osgi.SbtOsgi._

lazy val extra = <url>http://scalanlp.org/</url>
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
      <url>http://www.dlwh.org/</url>
    </developer>
  </developers>;

// git.useGitDescribe := true

val VersionRegex = "v([0-9]+.[0-9]+.[0-9]+)-?(.*)?".r


lazy val commonSettings = Seq(
  organization := "org.scalanlp",
  version := "0.4.4",
  /*
  git.baseVersion := "0.4",
  // append -SNAPSHOT unless we're on a branch
  git.gitUncommittedChanges := false,//git.gitCurrentTags.value.isEmpty,
  isSnapshot := false,
  git.gitTagToVersionNumber := { v: String =>
    v match {
      case VersionRegex(v,"") => Some(v)
      case VersionRegex(v,"SNAPSHOT") => Some(s"$v-SNAPSHOT")
      case VersionRegex(v,s) => Some(s"$v-$s-SNAPSHOT")
      case _ => None
    }
  },
  */
  scalaVersion := Version.scala,
  crossScalaVersions := Seq("2.12.1", "2.11.8"),
  libraryDependencies ++= Seq(
    Library.breeze,
    Library.breezeConfig,
    Library.mapdb,
    Library.slf4jSimple,
    Library.commonsLang3,
    Library.jflex,
    Library.scalatest % "test",
    Library.scalacheck % "test",
    Library.junit % "test"
  ),
  scalacOptions ++= Seq("-deprecation", "-language:_", "-optimize"),
  javaOptions += "-Xmx6g",
  //javaOptions += "-Xrunhprof:cpu=samples,depth=12",
  fork := true,
  publishMavenStyle := true,
  pomExtra := extra,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  assemblyOption in assembly ~= {
    _.copy(cacheOutput = false)
  },
  resolvers ++= Seq(
    "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
    "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  ),
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 12 => Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.6")
      case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.6")
      case _ => Seq.empty
      })
  ) ++ sbtjflex.SbtJFlexPlugin.jflexSettings
  // ++ net.virtualvoid.sbt.graph.Plugin.graphSettings
  
  // TODO rewrite in sbt 1.x syntax
  /* ++
  IndexedSeq (
      mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
        {
          case PathList("org", "w3c", "dom", _) => MergeStrategy.first
          case PathList("javax", "xml", "stream", _ *) => MergeStrategy.first
          case PathList("scala", "xml", _ *) => MergeStrategy.first
          case PathList("org", "cyberneko", "html", _ *) => MergeStrategy.first
          case x => old(x)
         }
      }
  )*/

lazy val epicCore = project
  .in(file("modules/core"))
  //.enablePlugins(GitVersioning)
  .settings(commonSettings)
  .settings(
    name := "epic"
    //,
    //OsgiKeys.privatePackage := Seq(),
    //OsgiKeys.importPackage := Seq("org.apache.tika.*; resolution:=optional", "*"),
    //OsgiKeys.exportPackage := Seq("epic.*")
  )

lazy val epicHtml = project
  .in(file("modules/html"))
  .dependsOn(epicCore)
  .settings(
    commonSettings,
    name := "epic-html",
    libraryDependencies ++= Seq(
       Library.tikaParsers,
       Library.boilerpipe,
       Library.nekohtml,
    )
  )
