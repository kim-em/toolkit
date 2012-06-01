addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0-M2")

libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-web-plugin" % (v+"-0.2.10"))

resolvers += Resolver.url("scalasbt",
  new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.jsuereth" % "xsbt-gpg-plugin" % "0.5")

// resolvers += "Web plugin repository" at "http://siasia.github.com/maven2"    

