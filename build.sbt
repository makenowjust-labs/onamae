Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalacOptions ++= Seq("-deprecation")

lazy val root = project
  .in(file("."))
  .settings(
    name := "onamae",
    scalaVersion := "3.5.2",
    console / initialCommands :=
      """|import onamae.{*, given}
         |import onamae.automaton.{*, given}
         |
         |def time[A](f: => A): (Long, A) =
         |  val start = System.currentTimeMillis
         |  val a = f
         |  val end = System.currentTimeMillis
         |  println(f"time: ${(end - start).toDouble / 1000}%.3f s")
         |  (end - start, a)
         |""".stripMargin,
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
  )
