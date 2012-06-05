name := "toolkit"

organization := "net.tqft"

version := "0.1.5"

//scalaVersion := "2.9.2"
scalaVersion := "2.10.0-M3"

// in order to compile against 2.10.0-M3, you need to run the hack described at http://www.scala-lang.org/node/12251
crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0-M3")

//scalacOptions += "-Xprint:typer"

//publishTo := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa")))

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { x => false }

pomExtra := (
  <url>http://bitbucket.org/scottmorrison/toolkit</url>
  <licenses>
    <license>
      <name>CC-BY</name>
      <url>http://creativecommons.org/licenses/by/3.0/</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>https://bitbucket.org/scottmorrison/toolkit</url>
  </scm>
  <developers>
    <developer>
      <id>scott</id>
      <name>Scott Morrison</name>
      <url>http://tqft.net</url>
    </developer>
  </developers>
)

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

