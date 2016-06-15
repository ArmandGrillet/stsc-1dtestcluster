organization := "gr.armand"
name := "stsc.1dtestcluster"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
    // Library for numerical processing.
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12",

    // The unit test library.
    "org.scalactic" %% "scalactic" % "2.2.6",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",

    // Libary for the user interface.
    "org.scalafx" %% "scalafx" % "8.0.92-R10",
    "org.scalafx" %% "scalafxml-core-sfx8" % "0.2.2",

    // Library for self-tuning spectral clustering.
    "gr.armand" %% "stsc" % "1.0"
)

// For @fxml
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// To display the Madena theme.
unmanagedJars in Compile += {
    val ps = new sys.SystemProperties
    val jh = ps("java.home")
    Attributed.blank(file(jh) / "lib/ext/jfxrt.jar")
}
