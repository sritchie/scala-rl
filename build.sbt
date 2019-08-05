val scalaTestVersion = "3.0.8"
val scalacheckVersion = "1.14.0"

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.samritchie",
      scalaVersion := "2.13.0"
    )),
    name := "functional-rl-in-scala"
  )

mainClass := Some("io.samritchie.rl.Main")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck" % scalacheckVersion % Test
)
