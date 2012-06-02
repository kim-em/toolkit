import sbt._

object Toolkit extends Build {
    lazy val root = Project(id = "toolkit",
                            base = file(".")) aggregate(base, arithmetic, amazon, collections, algebra, functions)

    lazy val base = Project(id = "toolkit.base",
                           base = file("base")) dependsOn()

    lazy val arithmetic = Project(id = "toolkit.arithmetic",
                           base = file("arithmetic")) dependsOn()

    lazy val amazon = Project(id = "toolkit.amazon",
                           base = file("amazon")) dependsOn()

    lazy val collections = Project(id = "toolkit.collections",
                           base = file("collections")) dependsOn(functions)

    lazy val algebra = Project(id = "toolkit.algebra",
                           base = file("algebra")) dependsOn(base, arithmetic)

    lazy val functions = Project(id = "toolkit.functions",
                           base = file("functions"))


}
