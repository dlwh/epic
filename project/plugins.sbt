//libraryDependencies ++= Seq(
//  "org.jacoco" % "org.jacoco.core" % "0.5.6.201201232323" artifacts(Artifact("org.jacoco.core", "jar", "jar")),
//  "org.jacoco" % "org.jacoco.report" % "0.5.6.201201232323" artifacts(Artifact("org.jacoco.report", "jar", "jar")))

//addSbtPlugin("de.johoop" % "jacoco4sbt" % "1.2.2")

resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.2")

addSbtPlugin("org.scalanlp" % "sbt-jflex" % "0.2")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.6.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")
