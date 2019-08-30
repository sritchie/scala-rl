/* dependency versions */
lazy val V = new {
  val algebird = "0.13.5"
  val cats = "1.1.0"
  val evilplot = "0.6.3"
  val kindProjector = "0.10.3"
  val rainier = "0.2.2"
  val scala = "2.12.8"
  val scalacheck = "1.14.0"
  val scalatest = "3.0.8"
  val util = "19.8.1"
}

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "io.samritchie",
      scalaVersion := V.scala
    )
  ),
  name := "functional-rl-in-scala"
)

mainClass in (Compile, run) := Some("io.samritchie.rl.Plot")

resolvers += Resolver.bintrayRepo("cibotech", "public")

libraryDependencies ++= Seq(
  // Charts.
  "com.cibo" %% "evilplot" % V.evilplot,
  "com.cibo" %% "evilplot-repl" % V.evilplot,
  // For the probability monad.
  "com.stripe" %% "rainier-cats" % V.rainier,
  "com.stripe" %% "rainier-core" % V.rainier,
  // For the monoids and implementations.
  "com.twitter" %% "algebird-core" % V.algebird,
  "com.twitter" %% "util-core" % V.util,
  // For its typeclasses, Monad specifically.
  "org.typelevel" %% "cats-core" % V.cats,
  "org.typelevel" %% "cats-free" % V.cats,
  // Testing.
  "org.scalatest" %% "scalatest" % V.scalatest % Test,
  "org.scalacheck" %% "scalacheck" % V.scalacheck % Test
) ++ Seq(compilerPlugin("org.typelevel" %% "kind-projector" % V.kindProjector))

// Lets me C-c out of the running process.
cancelable in Global := true
