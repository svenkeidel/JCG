name := "scala_2.12.6_invokedynamic_symbols"

scalaVersion := "2.12.6" // DO NOT UPDATE!

libraryDependencies += "de.opal-project" %% "jcg-annotations" % "1.0-SNAPSHOT"

Compile / scalacOptions ++= Seq(
    "-g:vars"
)

Compile / javacOptions ++= Seq(
    "-g:source,lines,vars"
)