package examples.crv15.breach.benchmark1

import rete._

/*
 * In the interval [0,50] the value of z is strictly less than 0.3. 
 * 
 * Spec: phi := alw_[0, 50] (z[t] < 0.3)
 * 
 * Events:
 * 
 * - z(time,value)
 */

// Use this map monitor:

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    map += 'kind -> 'z
    map += 'one -> record.get("time").toFloat
    map += 'two -> record.get("z").toFloat
    addMapEvent(map)
  }
}

class M extends CSVMonitor {
  val z = event
  val TimeOut = fact

  "r1" -- z('time, 'value) & not(TimeOut()) |-> {
    if ('time.float > 50)
      insert(TimeOut())
    else
      ensure("value < 0.3 in 0..50 interval")('value.float < 0.3)
  }
}

object Evaluate extends MonitorFeeder {
  val z = 'z
  
  val ptrace1 = List(
    z(1f, 0.299f),
    z(2f, 0.1f),
    z(1f, 0.299f),
    z(4f, 0.1f),
    z(5f, 0.299f),
    z(6f, 0.1f),
    z(7f, 0.299f),
    z(8f, 0.1f),
    z(51f, 0.9f),
    z(52f, 0.9f),
    z(53f, 0.9f))

  val ptrace_1 = List(
    z(1f, 0.299f),
    z(2f, 0.1f),
    z(1f, 0.299f),
    z(4f, 0.1f),
    z(5f, 0.3f),
    z(6f, 0.1f),
    z(7f, 0.299f),
    z(8f, 0.1f),
    z(51f, 0.9f),
    z(52f, 0.9f),
    z(53f, 0.9f))
        
  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    ptrace_1 foreach m.addMapEvent
    m.terminate()
  }
}
