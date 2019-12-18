package app

/**
  * Created by nkatz on 18/12/19.
  */

object HLEInterval {

  def apply(hleLine: String) = {
    val split = hleLine.split("\\|")
    val hle = split(0)
    // rendezVous, tugging
    if (Set("adrift", "aground", "atAnchor", "atAnchorOrMoored", "gap",
      "highSpeedNearCoast", "loitering", "lowSpeed", "maa", "moored", "speedGrThanMax",
      "speedLessThanMin", "stopped", "travelSpeed", "trawling", "trawlSpeed", "underWay", "unusualSpeed").contains(hle)) {
      val vessels = List(split(1))
      val value = split(2)
      val stime = split(3).toLong
      val etime = split(4).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else if (hle == "rendezVous" || hle == "tugging") {
      val vessels = List(split(1), split(2))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else if (hle == "withinArea") {
      //withinArea|923166|fishing|true|1448977130|1448977242
      val vessels = List(split(1))
      val value = split(2)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else throw new RuntimeException(s"Don't know what to do with $hleLine")
  }
}

class HLEInterval(val hle: String, val vessels: List[String], val value: String, val stime: Long, val etime: Long)

