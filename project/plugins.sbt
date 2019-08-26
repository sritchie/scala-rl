resolvers ++= Seq(
  "jgit-repo".at("https://download.eclipse.org/jgit/maven"),
  Resolver.url("bintray-sbt-plugin-releases", url("https://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
    Resolver.ivyStylePatterns
  )
)

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.0.0")
