name := "toolkit"

organization := "net.tqft.toolkit"

version := "0.1.4"

scalaVersion := "2.9.1"

// in order to compile against 2.10.0-M2, you need to run the hack described at http://www.scala-lang.org/node/12251
crossScalaVersions := Seq("2.9.0", "2.9.1", "2.9.2", "2.10.0-M2")

resolvers ++= Seq(
	"Java.net Maven2 Repository" at "http://download.java.net/maven/2/",
	"tqft.net Maven repository" at "http://tqft.net/releases",
	"Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	"Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases",
	"Scala Snapshots" at "http://scala-tools.org/repo-snapshots/"
)

libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
	"org.slf4j" % "slf4j-log4j12" % "1.6.1",
	"org.scalatest" % "scalatest_2.9.0" % "1.7.1" % "test",
	"commons-logging" % "commons-logging" % "1.1.1",
	"net.java.dev.jets3t" % "jets3t" % "0.8.1",
	"com.google.code.typica" % "typica" % "1.7.2",
	"com.google.guava" % "guava" % "r09",
   	"org.apache.commons" % "commons-math" % "2.2",
	"org.apfloat" % "apfloat" % "1.6.3"
)

libraryDependencies += "com.nicta" %% "scoobi" % "0.4.0-SNAPSHOT" % "provided"


libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
    	deps :+ ("org.scala-lang" % "scala-compiler" % sv)
}

publishTo := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa")))
