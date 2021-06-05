name := "auto-crop"
version := "0.1.0"

scalaVersion := "3.0.0"

scalacOptions ++= Seq(
    "-deprecation",
    "-explain",
    "-explain-types",
    "-feature",
    // "-language:adhocExtensions",
    // "-language:implicitConversions",
    "-language:postfixOps",
    // "-language:reflectiveCalls",
    // "-language:doTheseActuallyWork?",
    "-source:3.0",
    "-unchecked"
)

libraryDependencies += "org.openpnp" % "opencv" % "4.5.1-2"
// libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

mainClass := Some("com.worthlesscog.images.AutoCrop")
