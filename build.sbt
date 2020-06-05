name := "SMT"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions := List(
  "-encoding",
  "utf8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-target:jvm-1.8",
  "-language:_",
  "-Ymacro-annotations"
)

val catsVersion       = "2.0.0"
val catsEffectVersion = "2.0.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.2"

libraryDependencies += "org.typelevel" %% "cats-core"        % catsVersion
libraryDependencies += "org.typelevel" %% "cats-free"        % catsVersion
libraryDependencies += "org.typelevel" %% "cats-laws"        % catsVersion
libraryDependencies += "org.typelevel" %% "cats-laws"        % catsVersion % "test"
libraryDependencies += "org.typelevel" %% "cats-effect"      % catsEffectVersion


addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.0" cross CrossVersion.patch)
