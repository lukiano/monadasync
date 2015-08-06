import scalariform.formatter.preferences.{AlignParameters, AlignSingleLineCaseStatements}

organization := "io.atlassian"

name := "monadasync"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.5", "2.11.7")

homepage := Some(url("https://bitbucket.org/lleggieri/monadasync"))

val specs2Version = "3.6.4"
val scalazVersion = "7.1.3"
val scalacheckVersion = "1.12.2"
val junitVersion = "4.12"
val ssbindingVersion = "0.4.0"

libraryDependencies ++= Seq(
    "org.scalaz"     %% "scalaz-core"               % scalazVersion
  , "org.scalaz"     %% "scalaz-concurrent"         % scalazVersion
  , "org.scalaz"     %% "scalaz-effect"             % scalazVersion
  , "org.scalaz"     %% "scalaz-scalacheck-binding" % scalazVersion     % "test"
  , "org.specs2"     %% "specs2-core"               % specs2Version     % "test"
  , "org.specs2"     %% "specs2-junit"              % specs2Version     % "test"
  , "org.specs2"     %% "specs2-scalacheck"         % specs2Version     % "test"
  , "org.scalacheck" %% "scalacheck"                % scalacheckVersion % "test"
  , "org.typelevel"  %% "scalaz-specs2"             % ssbindingVersion  % "test"
  , "junit"          %  "junit"                     % junitVersion      % "test"
)

scalacOptions := Seq(
  "-deprecation"
  , "-unchecked"
  , "-feature"
  , "-language:_"
  , "-Xfatal-warnings"
  , "-Xlog-free-terms"
  , "-target:jvm-1.7"
  , "-Xlint"
  , "-Yno-adapted-args"
  , "-Ywarn-dead-code"
  , "-Ywarn-numeric-widen"
  , "-Ywarn-value-discard"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignParameters, true)

pomExtra := (
  <scm>
    <url>git@bitbucket.org:lleggieri/monadasync.git</url>
    <connection>scm:git:git@bitbucket.org:lleggieri/monadasync.git</connection>
    <developerConnection>scm:git:git@bitbucket.org:lleggieri/monadasync.git</developerConnection>
  </scm>
  <distributionManagement>
    <repository>
      <id>atlassian-public</id>
      <name>Atlassian Public Repository</name>
      <url>https://maven.atlassian.com/public</url>
    </repository>
    <snapshotRepository>
      <id>atlassian-public-snapshot</id>
      <name>Atlassian Public Snapshot Repository</name>
      <url>https://maven.atlassian.com/public-snapshot</url>
    </snapshotRepository>
  </distributionManagement>
  <developers>
    <developer>
      <id>lleggieri</id>
      <name>Luciano Leggieri</name>
      <email>lleggieri@atlassian.com</email>
      <organization>Atlassian</organization>
      <organizationUrl>http://www.atlassian.com</organizationUrl>
    </developer>
  </developers>
)

mappings in (Compile, packageBin) ++= Seq(
    file("LICENSE") -> "META-INF/LICENSE"
  , file("NOTICE")  -> "META-INF/NOTICE"
)

val nexus = "https://maven.atlassian.com/"
lazy val release = Some("releases" at nexus + "public")
lazy val snapshots = Some("snapshots" at nexus + "public-snapshot")
lazy val localM2 = Some(Resolver.file("localm2", Path.userHome / ".m2" / "repository"))

publishTo <<= version { (v: String) =>
  if (v.trim endsWith "SNAPSHOT")
    snapshots
  else
    release
}

sbtrelease.ReleasePlugin.autoImport.releaseCrossBuild := true