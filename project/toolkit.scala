import sbt._

object Toolkit extends Build {
    lazy val root = Project(id = "toolkit",
                            base = file(".")) dependsOn() aggregate()

    lazy val arithmetic = Project(id = "toolkit.arithmetic",
                           base = file("arithmetic")) dependsOn()

    lazy val algebra = Project(id = "toolkit.algebra",
                           base = file("algebra")) dependsOn(arithmetic)

    lazy val functions = Project(id = "toolkit.functions",
                           base = file("functions"))
}
