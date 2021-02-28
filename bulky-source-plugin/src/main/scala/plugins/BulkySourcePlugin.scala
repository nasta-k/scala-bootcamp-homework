package plugins

import sbt._
import sbt.Keys._

object BulkySourcePlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    val bulkySources = taskKey[Seq[(Int, File)]]("Sequence of files longer than 100 lines in descending order.")
    val bulkyThresholdInLines = settingKey[Int]("Threshold for lines")
  }

  import autoImport._

  override val projectSettings: Seq[Setting[_]] =
    inConfig(Compile)(bulkySourceSettings) ++ inConfig(Test)(bulkySourceSettings) ++ {
      bulkyThresholdInLines := 100
    }

  def bulkySourceSettings: Seq[Setting[_]] = Seq(
    bulkySources := getBulkyLinesFiles(sources.value))

  def getBulkyLinesFiles(files: Seq[File]): Seq[(Int, File)] = {
    files.map { file => (sbt.IO.readLines(file).length, file) }
      .filter { case (len, _) => len > bulkyThresholdInLines.value }
      .sortBy { case (len, _) => len }
      .reverse
  }

  //
  //  override val projectSettings: Seq[Setting[_]] = {
  //    bulkySources := getBulkyLinesFiles((sources in Compile).value, bulkyThresholdInLines.value)
  //    (Test / bulkySources) := getBulkyLinesFiles((sources in Test).value, bulkyThresholdInLines.value)
  //  }
}
