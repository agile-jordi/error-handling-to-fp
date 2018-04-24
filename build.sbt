
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

lazy val root = (project in file("."))
  .settings(
    name := "Hello",
    scalaVersion := "2.12.3",
//    wartremoverErrors ++= Warts.unsafe,
    wartremoverWarnings ++= Warts.allBut(Wart.DefaultArguments, Wart.Equals),
    libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.22",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  )
