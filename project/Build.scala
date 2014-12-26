import sbt._
import Keys._
import pl.project13.scala.sbt.SbtJmh._
import JmhKeys._

object ProjectEulerBuild extends Build {

  val JmhVersion = "1.4"

  val root =
    Project(
      id = "project-euler",
      base = file("."),
      settings =
        Seq(
          scalaVersion := "2.11.2",
          scalacOptions ++=
            Seq(
              "-unchecked",
              "-deprecation",
              "-feature",
              "-Xlint",
              "-language:reflectiveCalls"),
          testOptions in Test := Seq(Tests.Filter(s => s.endsWith("Spec"))),
          resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
          libraryDependencies ++=
            Seq(
              "org.openjdk.jmh" % "jmh-core" % JmhVersion,
              "org.specs2" %% "specs2" % "2.4.6" % "test",
              "org.scalacheck" %% "scalacheck" % "1.11.6" % "test")) ++
          jmhSettings ++
          Seq(version in Jmh := JmhVersion))
}
