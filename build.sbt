enablePlugins(ScalaJSPlugin)

name := "Diode Example"

scalaVersion := "2.11.7"

workbenchSettings

bootSnippet := "SimpleApp().main();"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "me.chrons" %%% "diode-react" % "0.5.1",
  "com.github.japgolly.scalajs-react" %%% "core" % "0.10.4",
  "com.github.japgolly.scalajs-react" %%% "extra" % "0.10.4"
)