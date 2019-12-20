package app

/**
  * Created by nkatz on 18/12/19.
  */

object HLEInterval {

  // For me if argument is absent the row has space
  def apply(hleLine: String) = {
    val split = hleLine.split("\\|")
    val hle = split(0)

    /*
    val yours_event_set = Set("adrift", "aground", "atAnchor", "atAnchorOrMoored", "gap",
      "highSpeedNearCoast", "loitering", "lowSpeed", "maa", "moored", "speedGrThanMax", "tuggingSpeed",
      "speedLessThanMin", "stopped", "travelSpeed", "trawling", "trawlSpeed", "underWay", "unusualSpeed")
    */

    val mine_hle_event_set = Set("highSpeedNC", "lowSpeed", "stopped", "proximity", "trawlingMovement",
      "gap", "loitering", "changingSpeed", "tuggingSpeed", "trawling",
      "anchoredOrMoored", "sarSpeed", "sar", "trawlSpeed", "underWay",
      "movingSpeed", "drifting", "sarMovement", "pilotBoarding")

    // rendezVous, tugging
    if (mine_hle_event_set.contains(hle)) {
      val vessels = List(split(1))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else if (hle == "rendezVous" || hle == "tugging"
      || hle == "proximity" || hle == "pilotBoarding") {
      val vessels = List(split(1), split(2))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else if (hle == "withinArea") {
      //withinArea|923166|fishing|true|1448977130|1448977242
      val vessels = List(split(1), split(2))
      val value = split(3)
      val stime = split(4).toLong
      val etime = split(5).toLong
      new HLEInterval(hle, vessels, value, stime, etime)

    } else throw new RuntimeException(s"Don't know what to do with $hleLine")
  }
}

class HLEInterval(val hle: String, val vessels: List[String], val value: String, val stime: Long, val etime: Long)

