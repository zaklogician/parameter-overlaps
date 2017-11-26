lazy val root = (project in file(".")).
  settings(
    name := "overlaps",
    version := "0.1.0",
    scalaVersion := "2.12.2",
    mainClass in Compile := Some("overlaps.Main")
  )

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies ++= Seq(
  "org.apache.commons" % "commons-math3" % "3.6.1",
  "org.scalacheck" %% "scalacheck" % "1.13.5" 
)
