package plugins

import sbt._
import sbt.Keys._

object BulkySourcePlugin extends AutoPlugin {
  override def trigger = allRequirements

  val bulkySources = taskKey[Seq[(Int, File)]]("Sequence of files longer than 100 lines in descending order.")

  val bulkyThresholdInLines = settingKey[Int]("Threshold for lines")

  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    bulkyThresholdInLines := 100
  )

  def getBulkyLinesFiles(files: Seq[File], threshold: Int): Seq[(Int, File)] = {
    files.map { file => (sbt.IO.readLines(file).length, file) }
      .filter { case (len, _) => len > threshold }
      .sortBy { case (len, _) => len }
      .reverse
  }

  override val projectSettings: Seq[Setting[_]] = {
    bulkySources := getBulkyLinesFiles((sources in Compile).value, bulkyThresholdInLines.value)
    (Test / bulkySources) := getBulkyLinesFiles((sources in Test).value, bulkyThresholdInLines.value)
  }
}
