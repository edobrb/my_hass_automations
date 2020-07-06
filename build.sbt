name := "MyHassAutomation"

version := "0.1"

scalaVersion := "2.12.11"

resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "com.github.edobrb" % "scala-hass-interface" % "8c2dd6a3c6"
libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.24.0"