import sbt._
import com.typesafe.sbt.SbtScalariform._
import Keys._
import wartremover.WartRemover.autoImport._

import scalariform.formatter.preferences.{ AlignParameters, AlignSingleLineCaseStatements }

object MonadAsyncBuild extends Build {

  object Version {
    val specs2 = "3.6.5"
    val scalaz = "7.1.5"
    val scalacheck = "1.12.2"
    val junit = "4.12"
    val ssbinding = "0.4.0"
    val scalazStream = "0.8"
  }

  object Repositories {
    val nexus = "https://maven.atlassian.com/"
    lazy val release = Some("releases" at nexus + "public")
    lazy val snapshots = Some("snapshots" at nexus + "public-snapshot")
    lazy val localM2 = Some(Resolver.file("localm2", Path.userHome / ".m2" / "repository"))
  }

  lazy val root =
    project.in(file("."))
      .aggregate(core, stream)
      .settings(
        organization := "io.atlassian",
        scalaVersion := "2.11.7",
        crossScalaVersions := Seq("2.10.5", "2.11.7"),
        homepage := Some(url("https://bitbucket.org/lleggieri/monadasync")),
        libraryDependencies ++= Seq(
          "org.scalaz"     %% "scalaz-core"               % Version.scalaz
          , "org.scalaz"     %% "scalaz-concurrent"         % Version.scalaz
          , "org.scalaz"     %% "scalaz-effect"             % Version.scalaz
          , "org.scalaz"     %% "scalaz-scalacheck-binding" % Version.scalaz     % "test"
          , "org.specs2"     %% "specs2-core"               % Version.specs2     % "test"
          , "org.specs2"     %% "specs2-junit"              % Version.specs2     % "test"
          , "org.specs2"     %% "specs2-scalacheck"         % Version.specs2     % "test"
          , "org.scalacheck" %% "scalacheck"                % Version.scalacheck % "test"
          , "org.typelevel"  %% "scalaz-specs2"             % Version.ssbinding  % "test"
          , "junit"          %  "junit"                     % Version.junit      % "test"
        ),
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
          ,
        credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
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
        addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3"),
        wartremoverErrors ++= Warts.all
      ).settings(scalariformSettings)

  lazy val core = project.in(file("core")).settings(
    name := "monadasync-core"
  )

  lazy val stream = project.in(file("stream")).dependsOn(core).settings(
    name := "monadasync-stream",
    libraryDependencies ++= Seq(
      "org.scalaz.stream"   %% "scalaz-stream"     % Version.scalazStream
    )
  )
}

