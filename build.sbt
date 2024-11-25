Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalacOptions ++= Seq("-deprecation")

lazy val root = project
  .in(file("."))
  .settings(
    name := "onamae",
    scalaVersion := "3.5.2",
    console / initialCommands :=
      """|import onamae.*
         |""".stripMargin,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
