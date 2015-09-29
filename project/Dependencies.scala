import sbt._

object Version {
  val scala               = "2.11.7"
  val scalaTest           = "2.2.5"
  val breeze              = "0.12-SNAPSHOT"
  val breezeConfig        = "0.9.1"
  val mapdb               = "0.9.2"
  val scalaLogging        = "2.1.2"
  val tikaParsers         = "1.5"
  val boilerpipe          = "1.1.0"
  val nekohtml            = "1.9.21" //needed by boilerpipe
  val slf4jSimple         = "1.7.6"
  val commonsLang3        = "3.3.2"
  val jflex               = "1.6.0"  // 1.6.1 is out, but somehow 1.6.0 is apparentlu included by the 0.3 plugin
  val scalatest           = "2.1.3"
  val scalacheck          = "1.11.3"
  val junit               = "4.5"

}

object Library {
  val breeze              = "org.scalanlp"               %% "breeze"                               % Version.breeze
  val breezeConfig        = "org.scalanlp"               %% "breeze-config"                        % Version.breezeConfig
  val mapdb               = "org.mapdb"                  %  "mapdb"                                % Version.mapdb
  val scalaLoggingSlf4j   = "com.typesafe.scala-logging" %% "scala-logging-slf4j"                  % Version.scalaLogging
  val tikaParsers         = ("org.apache.tika"           %  "tika-parsers"                         % Version.tikaParsers)
                              .exclude("edu.ucar", "netcdf").exclude("com.googlecode.mp4parser", "isoparser")
  val boilerpipe          = "de.l3s.boilerpipe"          %  "boilerpipe"                           % Version.boilerpipe
  val nekohtml            = "net.sourceforge.nekohtml"   %  "nekohtml"                             % Version.nekohtml //needed by boilerpipe
  val slf4jSimple         = "org.slf4j"                  %  "slf4j-simple"                         % Version.slf4jSimple
  val commonsLang3        = "org.apache.commons"         %  "commons-lang3"                        % Version.commonsLang3
  val jflex               = "de.jflex"                   %  "jflex"                                % Version.jflex
  val scalatest           = "org.scalatest"              %% "scalatest"                            % Version.scalaTest
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                           % Version.scalacheck
  val junit               = "junit"                      %  "junit"                                % Version.junit
}

