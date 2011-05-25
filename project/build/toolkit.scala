
import sbt._

class Toolkit(info: ProjectInfo) extends DefaultProject(info) {
  // publish to:
  override def managedStyle = ManagedStyle.Maven
  lazy val publishTo = Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa"))

  // extra repositories:
  val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"

  // compile options:
  override def compileOptions = super.compileOptions ++ compileOptions("-verbose")

  // dependencies:
  val commonsLogging = "commons-logging" % "commons-logging" % "1.1.1"
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % "2.9.0"
  val jetset = "net.java.dev.jets3t" % "jets3t" % "0.8.1"

  val scalatest = "org.scalatest" %% "scalatest" % "1.4.1" % "test"
  val junit = "junit" % "junit" % "4.8.1" % "test"

}

