import com.typesafe.tools.mima.plugin.MimaPlugin.mimaDefaultSettings
import microsites.{CdnDirectives, MicrositeEditButton}
import sbtorgpolicies.utils.getEnvVar
import wartremover.Wart

Global / onChangedBuildSource := ReloadOnSourceChanges

/* dependency versions */
lazy val V = new {
  val algebird = "0.13.6"
  val cats = "2.1.0"
  val evilplot = "0.7.0"
  val kindProjector = "0.10.3"
  val rainier = "0.2.3"
  val scala = "2.12.10"
  val scalacheck = "1.14.3"
  val scalatest = "3.1.0"
  val scalaTestPlus = "3.1.0.1"
  val util = "20.3.0"
}

lazy val docsSourcesAndProjects: Seq[ProjectReference] =
  Seq(rlCore, rlPlot, rlBook)

val compilerOptions = Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials"
)

// The console can't handle these.
val consoleExclusions = Seq(
  "-Ywarn-unused:imports", "-Xfatal-warnings", "-Xlint"
)

val ignoredWarts: Set[Wart] =
  Set(Wart.DefaultArguments, Wart.TraversableOps, Wart.Any, Wart.NonUnitStatements)

def unsafeWartsExcept(ws: Set[wartremover.Wart]): Seq[wartremover.Wart] =
  Warts.unsafe.filterNot(w => ws.exists(_.clazz == w.clazz))

val sharedSettings = Seq(
  organization := "io.samritchie",
  scalaVersion := V.scala,

  // Lets me C-c out of the running process.
  cancelable in Global := true,
  parallelExecution in Test := true,
  scalafmtOnCompile in ThisBuild := true,
  wartremoverErrors in (ThisBuild, compile) ++= unsafeWartsExcept(ignoredWarts),
  unmanagedBase in Global := baseDirectory.value / "lib",
  resolvers ++= Seq(
    Resolver.bintrayRepo("cibotech", "public")
  ),
  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-Xlint",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials"),


  scalacOptions in (Compile, console) --= consoleExclusions,
  scalacOptions in (Test, console) --= consoleExclusions,

  // Publishing options:
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  releaseVersionBump := sbtrelease.Version.Bump.Minor, // need to tweak based on mima results
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { x => false },

  publishTo := Some(
      if (version.value.trim.endsWith("SNAPSHOT"))
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    ),

  scmInfo := Some(
    ScmInfo(
      url("https://github.com/sritchie/scala-rl"),
      "scm:git@github.com:sritchie/scala-rl.git"
    )
  ),

  pomExtra := (
    <url>https://github.com/sritchie/scala-rl</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
        <comments>A business-friendly OSS license</comments>
      </license>
    </licenses>
    <developers>
      <developer>
        <id>sritchie</id>
        <name>Sam Ritchie</name>
        <url>https://www.samritchie.io</url>
      </developer>
    </developers>)
) ++ mimaDefaultSettings

lazy val noPublishSettings = Seq(
    publish := {},
    publishLocal := {},
    test := {},
    publishArtifact := false
)

/**
  * This returns the previous jar I've released that is compatible
  * with the current.
  */
val noBinaryCompatCheck = Set[String]("core")

def previousVersion(subProj: String) =
  Some(subProj)
    .filterNot(noBinaryCompatCheck.contains(_))
    .map { s => "io.sritchie" %% ("scala-rl-" + s) % "0.0.1" }

lazy val rl = Project(
  id = "scala-rl",
  base = file("."))
  .settings(sharedSettings)
  .settings(noPublishSettings)
  .aggregate(rlCore, rlBook, rlPlot, rlWorld)

def module(name: String) = {
  val id = "scala-rl-%s".format(name)
  Project(id = id, base = file(id)).settings(sharedSettings ++ Seq(
    Keys.name := id,
    mimaPreviousArtifacts := previousVersion(name).toSet)
  )
}

lazy val rlCore = module("core").settings(
  libraryDependencies ++= Seq(
     "com.stripe" %% "rainier-cats" % V.rainier,
     "com.stripe" %% "rainier-core" % V.rainier,

    // For the monoids and implementations.
    "com.twitter" %% "algebird-core" % V.algebird,
    "com.twitter" %% "util-core" % V.util,
    // For its typeclasses, Monad specifically.
    "org.typelevel" %% "cats-core" % V.cats,
    "org.typelevel" %% "cats-free" % V.cats,
    // Testing.
    "com.twitter" %% "algebird-test" % V.algebird % Test,
    "org.scalatest" %% "scalatest" % V.scalatest % Test,
    "org.scalacheck" %% "scalacheck" % V.scalacheck % Test,
    "org.scalatestplus" %% "scalacheck-1-14" % V.scalaTestPlus % Test
  ) ++ Seq(compilerPlugin("org.typelevel" %% "kind-projector" % V.kindProjector)),
)

lazy val rlWorld = module("world").settings(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % V.scalatest % Test,
    "org.scalacheck" %% "scalacheck" % V.scalacheck % Test
  )
).dependsOn(rlCore)

lazy val rlPlot = module("plot").settings(
  libraryDependencies ++= Seq(
    // Charts.
    "com.cibo" %% "evilplot" % V.evilplot,
    "com.cibo" %% "evilplot-repl" % V.evilplot,
  )
).dependsOn(rlCore)

lazy val rlBook = module("book").settings(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % V.scalatest % Test,
    "org.scalacheck" %% "scalacheck" % V.scalacheck % Test
  ),
  initialCommands :=
    """
import com.scalarl._
import com.stripe.rainier.sampler.RNG
import com.stripe.rainier.compute.{Evaluator, Real}

implicit val rng: RNG = RNG.default
implicit val evaluator: Numeric[Real] = new Evaluator(Map.empty)
""".stripMargin('|'),
  mainClass in (Compile, run) := Some("scalarl.book.Chapter2"),
).dependsOn(rlCore, rlPlot, rlWorld)

lazy val docsMappingsAPIDir = settingKey[String]("Name of subdirectory in site target directory for api docs")

lazy val docSettings = Seq(
  micrositeName := "ScalaRL",
  micrositeDescription := "Reinforcement Learning in Scala.",
  micrositeAuthor := "Sam Ritchie",
  micrositeUrl := "http://www.scalarl.com",
  micrositeDocumentationUrl := "/api/com/scalarl/index.html",

  micrositeHomepage := "http://www.scalarl.com/",
  micrositeOrganizationHomepage := "https://www.samritchie.io",
  micrositeTwitter := "@scalaRLProject",
  micrositeTwitterCreator := "@sritchie",

  micrositeGitterChannelUrl := "ScalaRL/community",
  micrositeGithubOwner := "sritchie",
  micrositeGithubRepo := "scala-rl",

  micrositeAnalyticsToken := "UA-146772284-1",

  micrositeExtraMdFiles := Map(
    file("CONTRIBUTING.md") ->
      microsites.ExtraMdFileConfig(
        "contributing.md",
        "page",
        Map("title" -> "Contributing", "section" -> "contributing", "position" -> "5")
      )),

  micrositeEditButton := Some(
    MicrositeEditButton(
      "Help us improve this page",
      "/edit/develop/docs/src/main/tut/{{ page.path }}")),

  micrositeHighlightTheme := "atom-one-light",
  micrositePalette := Map(
    "brand-primary" -> "#5B5988",
    "brand-secondary" -> "#292E53",
    "brand-tertiary" -> "#222749",
    "gray-dark" -> "#49494B",
    "gray" -> "#7B7B7E",
    "gray-light" -> "#E5E5E6",
    "gray-lighter" -> "#F4F3F4",
    "white-color" -> "#FFFFFF"),

  autoAPIMappings := true,
  unidocProjectFilter in (ScalaUnidoc, unidoc) :=
    inProjects(docsSourcesAndProjects:_*),
  docsMappingsAPIDir := "api",
  addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), docsMappingsAPIDir),
  ghpagesNoJekyll := false,

  // Don't kill the cname redirect.
  excludeFilter in ghpagesCleanSite :=
    new FileFilter{
      def accept(f: File) = (ghpagesRepository.value / "CNAME").getCanonicalPath == f.getCanonicalPath
    } || "versions.html",

  fork in tut := true,
  fork in (ScalaUnidoc, unidoc) := true,
  scalacOptions in (ScalaUnidoc, unidoc) ++= Seq(
    "-doc-source-url", "https://github.com/sritchie/scala-rl/tree/develop€{FILE_PATH}.scala",
    "-sourcepath", baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    // "-diagrams", // Not working locally; wait until graphviz is reliable.
    "-doc-root-content", "scaladoc-root.txt"
  ),
  git.remoteRepo := "git@github.com:sritchie/scala-rl.git"
)

// Documentation is generated for projects defined in
// `docsSourcesAndProjects`.
lazy val docs = project
  .enablePlugins(MicrositesPlugin, TutPlugin, ScalaUnidocPlugin, GhpagesPlugin)
  .settings(moduleName := "scala-rl-docs")
  .settings(sharedSettings)
  .settings(noPublishSettings)
  .settings(docSettings)
  .settings((scalacOptions in Tut) ~= (_.filterNot(Set("-Ywarn-unused-import", "-Ywarn-dead-code"))))
  .dependsOn(rlCore, rlPlot, rlBook, rlWorld)
