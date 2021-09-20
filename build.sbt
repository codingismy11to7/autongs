name := "autongs"

(ThisBuild / version) := "0.1.2"

(ThisBuild / scalaVersion) := "2.13.6"

(ThisBuild / scalafmtOnCompile) := true

val sjsReact = "2.0.0-RC3"
val react    = "17.0.2"
//val react = "16.13.1"
val mui = "4.12.3"

//val mui = "3.9.0"

lazy val `core-ext-zio` = project
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-prelude" % "1.0.0-RC6" exclude ("io.github.cquiroz", "scala-java-time-tzdb_sjs1_2.13"),
//      "dev.zio" %%% "zio" % "1.0.11" exclude ("io.github.cquiroz", "scala-java-time-tzdb_sjs1_2.13"),
      "io.github.cquiroz" %%% "scala-java-time" % "2.2.0",
//      "io.github.cquiroz"                 %%% "scala-java-time-tzdb" % "2.2.0",
      "com.github.japgolly.scalajs-react" %%% "core-generic"        % sjsReact,
      "com.github.japgolly.scalajs-react" %%% "util-dummy-defaults" % sjsReact % Provided,
    )
  )
  .enablePlugins(ScalaJSPlugin)

lazy val autongs = project
  .in(file("."))
  .dependsOn(`core-ext-zio`)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
//      "com.github.japgolly.scalajs-react" %%% "core-bundle-cats_effect" % sjsReact,
//      "com.github.japgolly.scalajs-react" %%% "core" % sjsReact,
      "io.kinoplan" %%% "scalajs-react-material-ui-core" % "0.2.1+98-f4223ad2-SNAPSHOT"
    ),
    (Compile / npmDependencies) ++= Seq(
      "@material-ui/core" -> mui,
      "react"             -> react,
      "react-dom"         -> react,
    ),
    webpackConfigFile := Some(baseDirectory.value / "webpack.config.js"),
    webpackEmitSourceMaps := false,
//    (fastOptJS / scalaJSLinkerConfig) ~= (_.withOptimizer(false)),
  )
  .enablePlugins(ScalaJSBundlerPlugin)

val buildOpt = taskKey[Seq[File]]("Builds fullOpt version")
buildOpt := Bundle.createOptTask.value

val buildFast = taskKey[Seq[File]]("Builds fastOpt version")
buildFast := Bundle.createFastTask.value
