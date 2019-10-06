resolvers ++= Seq(
  "jgit-repo".at("https://download.eclipse.org/jgit/maven"),
  Resolver.url("bintray-sbt-plugin-releases", url("https://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
    Resolver.ivyStylePatterns
  )
)

// good
addSbtPlugin("com.47deg"  % "sbt-microsites" % "0.9.7")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")
addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.11")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.0")
addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.3")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.0.6")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.3")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.8")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.3.7")
