package app

/**
  * Created by nkatz on 18/12/19.
  */

import java.io.{BufferedWriter, File, FileWriter}
import intervalTree.IntervalTree
import data._
import scala.collection.mutable.ListBuffer
import scala.io.Source

import java.time._
// Reads data in this format:
/*change_in_heading|1443694493|1443694493|245257000
change_in_speed_start|1443890320|1443890320|259019000
change_in_speed_end|1443916603|1443916603|311018500
coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
entersArea|1443875806|1443875806|228017700|18515
leavesArea|1443887789|1443887789|235113366|21501
gap_start|1444316024|1444316024|246043000
gap_end|1445063602|1445063602|269103970
proximity|1445357304|1445354425|1445357304|true|227519920|227521960
velocity|1443665064|1443665064|228374000|19.30468943370810563920|31.39999999999999857891|35.54927442329611153582
slow_motion_start|1444262324|1444262324|228167900
slow_motion_end|1444281397|1444281397|258088080
stop_start|1444031990|1444031990|227705102
stop_end|1444187303|1444187303|259019000*/

object IntervalHandler extends App {

  // The path to the folder with RTEC results with HLE intervals. The generateIntervalTree
  // methdds reads from those files (see the method).
  val pathToHLEIntervals = "/home/manosl/Desktop/BSc Thesis/Datasets/IntervalHandlerInputDatasets/HLEs"

  // The path to the critical points (LLEs)
  val pathToLLEs = "/home/manosl/Desktop/BSc Thesis/Datasets/IntervalHandlerInputDatasets/LLEs.csv"

  println("Generating intervals tree...")

  val intervalTree =
    generateIntervalTree(
      pathToHLEIntervals,
      List("rendezVous", "stopped", "lowSpeed", "proximity")
    )

  readDataIntoMiniBatches(pathToLLEs, 10, "rendezVous", "asp")

  // mode is either "asp" or "mln"
  /**
    * Parses the input data into logical syntax and generates data mini-batches for training.
    *
    * A data batch is a chunk of input data of given size. Size is measured by temporal duration,
    * so given batchSize = n, a mini-batch consists of input data with time stamps t to t+n.
    *
    */
  def readDataIntoMiniBatches(dataPath: String, batchSize: Int, targetEvent: String, mode: String) = {

    val f = new File("./results")
    val writeToFile = new BufferedWriter(new FileWriter(f))

    val data = Source.fromFile(dataPath).getLines.filter(x =>
      !x.startsWith("coord") && !x.startsWith("velocity") && !x.startsWith("entersArea") && !x.startsWith("leavesArea")
    )

    val currentBatch = new ListBuffer[String]
    var timesAccum = scala.collection.mutable.SortedSet[Long]()
    var llesAccum = scala.collection.mutable.SortedSet[String]()
    var batchCount = 0

    var prev_batch_timestamp: Long = 0
    val INF_TS = 2000000000

    while (data.hasNext) {
      val newLine = data.next()

      val split = newLine.split("\\|")
      println(split.mkString(" "))

      val time = split(1)
      val lle = split(0)

      if (!timesAccum.contains(time.toLong)) timesAccum += time.toLong

      if (!llesAccum.contains(lle)) llesAccum += lle

      if ((timesAccum.size <= batchSize) && (data.hasNext)) {
        currentBatch += generateLLEInstances(newLine, mode)
      } else {
        currentBatch += generateLLEInstances(newLine, mode)
        batchCount += 1

        //what is the use of this line?
        val nexts = timesAccum.sliding(2).map(x => if (mode == "asp") s"next(${x.last},${x.head})" else s"next(${x.last},${x.head})")
        val intervals = if (data.hasNext) intervalTree.range(prev_batch_timestamp, timesAccum.last) else intervalTree.range(prev_batch_timestamp, INF_TS)

        timesAccum += prev_batch_timestamp

        if (!data.hasNext) timesAccum += INF_TS

        prev_batch_timestamp = timesAccum.last

        var extras = timesAccum.flatMap{ timeStamp =>
          val containedIn = intervals.filter(interval => (interval._3.stime < timeStamp && timeStamp < interval._3.etime))
          containedIn.map(x => HLEIntervalToAtom(x._3, timeStamp.toString, targetEvent))
        }

        extras = extras ++ intervals.map((interval) => if (interval._3.stime >= timesAccum.head) HLEIntervalToAtom(interval._3, interval._3.stime.toString, targetEvent) else "None")
        extras = extras ++ intervals.map((interval) => if (interval._3.etime <= timesAccum.last) HLEIntervalToAtom(interval._3, interval._3.etime.toString, targetEvent) else "None")

        // Why this line is used?
        if (extras.nonEmpty) {
          val stop = "stop"
        }

        for (x <- extras) currentBatch += x
        for (x <- nexts) currentBatch += x

        writeToFile.write(currentBatch.filter(x => x != "None").mkString(" ") + "\n")

        println(batchCount)
        currentBatch.clear()
        timesAccum.clear()
      }
    }

    println(s"All batches: $batchCount")
    println(s"LLEs: $llesAccum")
    writeToFile.close()
  }

  // mode is either "asp" or "mln"
  def generateLLEInstances(line: String, mode: String) = {
    // These have a different schema
    // proximity is HLE
    val abnormalLLEs = Set[String]("coord", "entersArea", "leavesArea", "velocity")
    val split = line.split("\\|")
    if (!abnormalLLEs.contains(split(0))) {

      // These have a common simple schema:
      // change_in_heading, change_in_speed_start, change_in_speed_end,
      // gap_start, gap_end, slow_motion_start, slow_motion_end, stop_start, stop_end
      val lle = split(0)
      val time = split(1)
      val vessel = split(3)
      if (mode == "asp") s"happensAt($lle($vessel),$time)" else s"HappensAt(${lle.capitalize}_$vessel),$time)"
    } else {

      if (split(0) == "coord") {
        //coord|1443686670|1443686670|228041600|-4.47298500000000043286|48.38163999999999731472
        /*
        val lle = split(0)
        val time = split(1)
        val vessel = split(3)
        val lon = split(4)
        val lat = split(5)
        // Don't return nothing in the MLN case (can't handle the coords syntax)
        if ("mode" == "asp") s"happensAt($lle($vessel,$lon,$lat),$time)" else ""
        */
        // do nothing (we won't use coord).
        "None"
      } else if (split(0) == "entersArea" || split(0) == "leavesArea") {
        //entersArea|1443875806|1443875806|228017700|18515
        // For me enters and leaves are has the form
        //['Fluent','Timestamp','Area_ID','Vessel_ID']

        val lle = split(0)
        val time = split(1)
        val vessel = split(3)
        val area = split(2)
        if (mode == "asp") s"happensAt($lle($vessel,$area),$time)"
        else s"HappensAt(${lle.capitalize}_${vessel}_$area,$time)"

      } else if (split(0) == "velocity") {

        // do nothing (we won't use velocity)
        "None"

      } else {
        throw new RuntimeException(s"Unexpected event: $line")
      }
    }
  }

  /* Generate an HLE logical atom. The i var carries all the info, the t var is the particular
   * time point of the generated atom. "target" is the name of the target complex event. The
   * target event is turned into a holdsAt predicate, while all others are turned into happensAt predicates. */
  def HLEIntervalToAtom(i: HLEInterval, t: String, target: String) = {

    val functor = if (i.hle == target) "holdsAt" else "happensAt"

    val fluentTerm =
      if (i.value != "true") s"${i.hle}(${(i.vessels :+ i.value).mkString(",")})"
      else s"${i.hle}(${i.vessels.mkString(",")})"

    s"$functor($fluentTerm,$t)"
  }

  def generateIntervalTree(pathToHLEs: String, interestedIn: List[String]) = {

      def getListOfFiles(dir: String): List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
          println(d.listFiles.toList)
          d.listFiles.filter(f => f.isFile).toList
          // This line does not suits me
          // d.listFiles.filter(f => f.isFile && interestedIn.exists(eventName => f.getName.contains(eventName))).toList
        } else {
          List[File]()
        }
      }

    val files = getListOfFiles(pathToHLEs).map(_.getCanonicalPath)

    val intervalTree = new IntervalTree[HLEInterval]()

    var counter = 0
    files foreach { file =>
      println(s"Updating interval tree from $file")

      val data = Source.fromFile(file).getLines.filter(line => interestedIn.contains(line.split("\\|")(0)))

      while (data.hasNext) {
        val newLine = data.next()
        val i = HLEInterval(newLine)

        counter += 1

        intervalTree.addInterval(i.stime, i.etime, i)
      }
    }

    intervalTree
  }

}
