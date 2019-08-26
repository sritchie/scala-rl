val scalaTestVersion = "3.0.8"
val scalacheckVersion = "1.14.0"
val breezeVersion = "1.0"

/* dependency versions */
lazy val V = new {
  val algebird = "0.13.5"
  val breeze = "1.0"
  val cats = "1.1.0"
  val evilplot = "0.6.0"
  val rainier = "0.2.2"
  val scala = "2.12.8"
  val scalacheck = "1.14.0"
  val scalatest = "3.0.8"
}

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "io.samritchie",
      scalaVersion := V.scala
    )),
    name := "functional-rl-in-scala"
  )

mainClass in (Compile, run) := Some("io.samritchie.rl.Main")

libraryDependencies ++= Seq(
  // For the probability monad.
  "com.stripe" %% "rainier-core" % V.rainier,

  // For the monoids and implementations.
  "com.twitter" %% "algebird-core" % V.algebird,

  // For graphs... but evilplot looks way better. I'm sure I'll want
  // to replace breeze-viz with that.
  "org.scalanlp" %% "breeze-viz" % V.breeze,

  // For its typeclasses, Monad specifically.
  "org.typelevel" %% "cats-core" % V.cats,

  // Testing.
  "org.scalatest" %% "scalatest" % V.scalatest % Test,
  "org.scalacheck" %% "scalacheck" % V.scalacheck % Test
)

// Lets me C-c out of the running process.
cancelable in Global := true
