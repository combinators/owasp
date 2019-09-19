import play.sbt.PlayLayoutPlugin
import play.twirl.sbt.SbtTwirl

lazy val commonSettings = Seq(
  version := "1.0.0-SNAPSHOT",
  organization := "org.combinators",
  
  scalaVersion := "2.12.9",

  resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases")
  ),

  scalacOptions ++= Seq(
    "-unchecked",
    "-deprecation",
    "-feature",
    "-language:implicitConversions"
  ),

  libraryDependencies ++= Seq(
    "org.combinators" %% "cls-scala" % "2.0.0+12-8d994c6b",
    "org.combinators" %% "templating" % "1.0.0+3-bee373e9",
    "org.combinators" %% "cls-scala-presentation-play-git" % "1.0.0-RC1+8-63d5cf0b",
    "org.scalameta" %% "scalameta" % "3.4.0",
    "org.scalameta" %% "contrib" % "3.4.0",
    "com.h2database" % "h2" % "1.4.196",
    "org.scalactic" %% "scalactic" % "3.0.1" % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    guice
  )

)

lazy val root = (Project(id = "owasp", base = file(".")))
  .settings(commonSettings: _*)
  .enablePlugins(SbtTwirl)
  .enablePlugins(PlayScala)
  .disablePlugins(PlayLayoutPlugin)
  .settings(
    moduleName := "owasp",

    sourceDirectories in (Compile, TwirlKeys.compileTemplates) := Seq(
      sourceDirectory.value / "main" / "java-templates",
    ),
    TwirlKeys.templateFormats += ("java" -> "org.combinators.templating.twirl.JavaFormat"),
    TwirlKeys.templateImports := Seq(),
    TwirlKeys.templateImports += "org.combinators.templating.twirl.Java",
    TwirlKeys.templateImports += "com.github.javaparser.ast._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.body._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.comments._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.expr._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.stmt._",
    TwirlKeys.templateImports += "com.github.javaparser.ast.`type`._",

    PlayKeys.playMonitoredFiles ++= (sourceDirectories in (Compile, TwirlKeys.compileTemplates)).value
  )

