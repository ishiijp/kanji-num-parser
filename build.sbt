name := "kanji-num-parser"

version := "1.0"

libraryDependencies ++= Seq(
   "org.scalaz" %% "scalaz-core" % "6.0.3"
)


resolvers ++= Seq(
  "java m2" at "http://download.java.net/maven/2",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
)
