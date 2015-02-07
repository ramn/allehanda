enablePlugins(JavaAppPackaging)

lazy val root = Project(
  id="root",
  base=file("."),
  settings=Seq(
    name := "allehanda",
    organization := "se.ramn",
    executableScriptName := "run",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.4",
    scalacOptions ++= Seq(
      "-feature",
      "-unchecked",
      "-deprecation",
      "-encoding", "utf8",
      //"-language:existentials",
      //"-language:higherKinds",
      //"-language:implicitConversions",
      //"-Xfatal-warnings",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xfuture",
      "-Ywarn-unused-import"
    ),
    javacOptions ++= Seq("-Xlint:unchecked"),
    resolvers += "Typesafe releases" at "http://repo.typesafe.com/typesafe/releases",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.2" % "test",
      "org.scalamock" %% "scalamock-scalatest-support" % "3.1.2" % "test"
    )
  )
)
