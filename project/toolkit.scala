import sbt._
import Keys._

object Toolkit extends Build {
    import BuildSettings._

    lazy val root = Project(id = "toolkit",
                            base = file("."),
 			    settings = buildSettings
	) aggregate(base, arithmetic, amazon, collections, algebra, functions, eval)

    lazy val base: Project = Project(id = "toolkit.base",
                           base = file("base"),
			   settings = buildSettings
	) dependsOn()

    lazy val arithmetic = Project(id = "toolkit.arithmetic",
                           base = file("arithmetic"),
			   settings = buildSettings
	) dependsOn()

    lazy val amazon = Project(id = "toolkit.amazon",
                           base = file("amazon"),
			   settings = buildSettings
	) dependsOn(base,collections)

    lazy val collections = Project(id = "toolkit.collections",
                           base = file("collections"),
			   settings = buildSettings
	) dependsOn(base, functions)

    lazy val algebra = Project(id = "toolkit.algebra",
                           base = file("algebra"),
			   settings = buildSettings
	) dependsOn(base, arithmetic, functions, collections)

    lazy val functions = Project(id = "toolkit.functions",
                           base = file("functions"),
                           settings = buildSettings
        )

    lazy val eval = Project(id = "toolkit.eval",
                           base = file("eval"),
                           settings = buildSettings
        )

}

object BuildSettings {
  import Resolvers._

  val buildOrganization = "net.tqft"
  val buildVersion      = "0.1.6"
  val buildScalaVersion = "2.10.0-M3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    publishTo    := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa"))),
    resolvers    := sonatypeResolvers
  )
}

object Resolvers {
	val sonatypeResolvers = Seq(
	        "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
	        "Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases"
	)
}
