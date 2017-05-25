package examples.crv15.acsvreader.scala

// -----------------
// ---  LogFire: ---
// -----------------

class Monitor {
  val PRINT = true

  val nameOfPosition: Map[Int, Symbol] =
    Map(1 -> 'one, 2 -> 'two, 3 -> 'three, 4 -> 'four, 5 -> 'five,
      6 -> 'six, 7 -> 'seven, 8 -> 'eight, 9 -> 'nine)

  def addMapEvent(map: Map[Symbol, Any]) {
    if (PRINT) {
      for ((key, value) <- map) print(s"$key = $value ")
      println()
    }
  }

  def terminate() {
    println("END")
  }
}


// ---------------------
// --- The property: ---
// ---------------------

class M extends Monitor 


// ------------------------
// --- Running LogFire: ---
// ------------------------

object Main {
  import org.apache.commons.csv._

  implicit def extendMonitor(monitor: Monitor) = new {
    def submit(record: CSVRecord) {
      var map: Map[Symbol, Any] = Map();
      map += 'kind -> record.get(0)
      for (i <- 1 until record.size()) {
        map += monitor.nameOfPosition(i) -> record.get(i)
      }
      monitor.addMapEvent(map);
    }
  }

  def main(args: Array[String]) {
    import java.io._
    import scala.collection.JavaConverters._

    val monitor = new M
    val in: Reader = new BufferedReader(new FileReader(args(1)))
    val records: Iterable[CSVRecord] = CSVFormat.DEFAULT.withHeader().parse(in).asScala
    records foreach monitor.submit
    monitor.terminate()
    in.close()
  }
}
