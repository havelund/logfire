package examples.nfer

import rete._

class MapMonitor extends Monitor {
  val e = event
  
  "r" -- e('m) |-> {
    println(getBinding('m).asInstanceOf[Map[Int,String]](1))
  }
}

object Main {
  def main(args: Array[String]) {
    val m = new MapMonitor

    m.PRINT = true
    m.addEvent('e)(Map(1 -> "one"))
  }
}