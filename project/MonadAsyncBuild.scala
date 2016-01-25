import sbt._
import com.typesafe.sbt.SbtScalariform._
import Keys._
import wartremover.WartRemover.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

import scalariform.formatter.preferences.{ AlignParameters, AlignSingleLineCaseStatements }

object MonadAsyncBuild extends Build {

  object Version {
    val commonsIO = "2.4"
    val imp = "0.2.0"
    val specs2 = "3.6.6"
    val scalaTest = "3.0.0-M15"
    val scalaz = "7.1.4"
    val cats = "0.3.0"
    val junit = "4.12"
    val ssbinding = "0.4.0"
    val scalazStream = "0.8"
    val twitterUtil = "6.30.0"
    val scodecScalaz = "1.1.0"
    val scodecStream = "0.11.0"
    val scalax = "0.3"
  }

  object Repositories {
    val nexus = "https://maven.atlassian.com/"
    lazy val release = Some("releases" at nexus + "private")
    lazy val snapshots = Some("snapshots" at nexus + "private-snapshot")
    lazy val localM2 = Some(Resolver.file("localm2", Path.userHome / ".m2" / "repository"))
  }

  object Common {
    def dependencies(version: String) = Seq(
        "org.spire-math"    %% "imp"                       % Version.imp          % "provided"
      , "org.scala-lang"     % "scala-reflect"             % version              % "provided"
      , "org.scalaz"        %% "scalaz-scalacheck-binding" % Version.scalaz       % "test"
      , "org.specs2"        %% "specs2-core"               % Version.specs2       % "test"
      , "org.specs2"        %% "specs2-junit"              % Version.specs2       % "test"
      , "org.specs2"        %% "specs2-scalacheck"         % Version.specs2       % "test"
      , "org.typelevel"     %% "scalaz-specs2"             % Version.ssbinding    % "test"
      , "org.scalaz.stream" %% "scalaz-stream"             % Version.scalazStream % "test"
      , "junit"              % "junit"                     % Version.junit        % "test"
    )
    def settings: Seq[Setting[_]] = Seq(
      organization := "io.monadasync",
      scalaVersion := "2.11.7",
      crossScalaVersions := Seq("2.10.5", "2.11.7"),
      homepage := Some(url("https://bitbucket.org/lleggieri/monadasync")),
      resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
      libraryDependencies ++= dependencies(scalaVersion.value),
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
      ),
      pomExtra :=
        <scm>
          <url>git@bitbucket.org:lleggieri/monadasync.git</url>
          <connection>scm:git:git@bitbucket.org:lleggieri/monadasync.git</connection>
          <developerConnection>scm:git:git@bitbucket.org:lleggieri/monadasync.git</developerConnection>
        </scm>
        <developers>
          <developer>
            <id>lleggieri</id>
            <name>Luciano Leggieri</name>
            <email>230980@gmail.com</email>
          </developer>
        </developers>
      ,
      ScalariformKeys.preferences := ScalariformKeys.preferences.value.setPreference(AlignSingleLineCaseStatements, true).setPreference(AlignParameters, true),
      mappings in (Compile, packageBin) ++= Seq(
          file("LICENSE") -> "META-INF/LICENSE"
        , file("NOTICE")  -> "META-INF/NOTICE"
      ),
      publishTo <<= version { (v: String) =>
        if (v.trim endsWith "SNAPSHOT")
          Repositories.snapshots
        else
          Repositories.release
      },
      sbtrelease.ReleasePlugin.autoImport.releaseCrossBuild := true,
      autoCompilerPlugins := true,
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
      parallelExecution in Test := false,
      testOptions in Test += Tests.Argument(TestFrameworks.Specs2, "console", "junitxml")
//      wartremoverErrors ++= Warts.unsafe
    ) ++ scalariformSettings
  }

  lazy val root =
    project.in(file("."))
      .settings(Common.settings)
      .settings(
        name := "monadasync-root"
      )
      .aggregate(coreJVM, coreJS, stream, twitter, catsJVM, catsJS, monixJVM, monixJS)

  lazy val core = crossProject
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(Common.settings:_*)
    .settings(
      name := "monadasync-core"
    )
  lazy val coreJVM = core.jvm
  lazy val coreJS = core.js

  // JVM only
  lazy val scalaz = project
    .in(file("scalaz"))
    .dependsOn(coreJVM % "test->test;compile->compile")
    .settings(Common.settings)
    .settings(
      name := "monadasync-scalaz",
      libraryDependencies ++= Seq(
          "org.scalaz"        %% "scalaz-core"               % Version.scalaz % "provided"
        , "org.scalaz"        %% "scalaz-concurrent"         % Version.scalaz % "provided"
        , "org.scalaz"        %% "scalaz-effect"             % Version.scalaz % "provided"
      )
    )

  lazy val cats = crossProject
    .crossType(CrossType.Pure)
    .in(file("cats"))
    .dependsOn(core % "test->test;compile->compile")
    .settings(Common.settings:_*)
    .settings(
      name := "monadasync-cats",
      libraryDependencies ++= Seq(
          "org.spire-math"        %%% "cats-core"             % Version.cats      % "provided"
        , "org.spire-math"        %%% "cats-macros"           % Version.cats      % "provided"
        , "org.spire-math"        %%% "cats-free"             % Version.cats      % "provided"
        , "org.spire-math"        %%% "cats-state"            % Version.cats      % "provided"
        , "org.spire-math"        %%% "cats-laws"             % Version.cats      % "provided"
        , "org.scalactic"         %%% "scalactic"             % Version.scalaTest % "test"
        , "org.scalatest"         %%% "scalatest"             % Version.scalaTest % "test"
  )
    )
  lazy val catsJVM = cats.jvm
  lazy val catsJS = cats.js

  // JVM only
  lazy val stream = project
    .in(file("stream"))
    .dependsOn(scalaz % "test->test;compile->compile")
    .settings(Common.settings)
    .settings(
      name := "monadasync-stream",
      libraryDependencies ++= Seq(
          "org.scalaz.stream" %% "scalaz-stream"             % Version.scalazStream % "provided"
        , "org.scodec"        %% "scodec-scalaz"             % Version.scodecScalaz % "provided"
        , "org.scodec"        %% "scodec-stream"             % Version.scodecStream % "provided"
        , "commons-io"         % "commons-io"                % Version.commonsIO    % "test"
      )
    )

  lazy val monix = crossProject
    .crossType(CrossType.Pure)
    .in(file("monix"))
    .dependsOn(cats % "test->test;compile->compile")
    .settings(Common.settings:_*)
    .settings(
      name := "monadasync-monix",
      libraryDependencies ++= Seq(
          "org.spire-math"     %%% "cats-core"                % Version.cats      % "provided"
        , "org.monifu"         %%% "scalax-atomic"            % Version.scalax    % "provided"
        , "org.monifu"         %%% "monifu"                   % "1.0"             % "provided"
      )
    )
  lazy val monixJVM = monix.jvm
  lazy val monixJS = monix.js

  // JVM only
  lazy val twitter = project
    .in(file("twitter"))
    .dependsOn(scalaz % "test->test;compile->compile").dependsOn(stream % "test->test;compile->compile")
    .settings(Common.settings)
    .settings(
      name := "monadasync-twitter",
      libraryDependencies ++= Seq(
          "org.scalaz"        %% "scalaz-core"               % Version.scalaz       % "provided"
        , "org.scalaz"        %% "scalaz-concurrent"         % Version.scalaz       % "provided"
        , "org.scalaz"        %% "scalaz-effect"             % Version.scalaz       % "provided"
        , "com.twitter"       %% "util-core"                 % Version.twitterUtil  % "provided"
        , "org.scodec"        %% "scodec-scalaz"             % Version.scodecScalaz % "test"
        , "org.scodec"        %% "scodec-stream"             % Version.scodecStream % "test"
      )
    )
}

