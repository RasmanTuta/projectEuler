//Project properties
//Thu Jun 09 17:39:45 CEST 2011

organization := "rasmantuta"

name := "projectEuler"

sbtVersion := "0.10.1"

version := "1.0"

scalaVersion := "2.9.1"


libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.6.1",
    "org.specs2" %% "specs2-scalaz-core" % "6.0.1" % "test"
)

resolvers ++= Seq("snapshots" at "http://scala-tools.org/repo-snapshots",
                    "releases"  at "http://scala-tools.org/repo-releases")
