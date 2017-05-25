package examples.crv15.rvmonitor.benchmark2

import rete._

/*
 * SafeFileWriter : This property captures the situation in which a program writes 
 * to a file after it is closed. 
 * 
 * Events:
 * 
 * - open(file)
 * - write(file)
 * - close(file) 
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  // event, Map, Collection, Iterator

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map()
    map += 'kind -> Symbol(record.get(0))
    map += 'one -> record.get(1).trim
    addMapEvent(map)
  }
}

class M_Slow extends CSVMonitor {
  val open, write, close = event
  val Open, Closed = fact

  "r1" -- open('file) |-> insert(Open('file))

  "r2" -- Open('file) & close('file) |->
    replace(Open)(Closed('file))

  "r3" -- Closed('file) & write('file) |->
    fail("write to closed file")

  "r4" -- Closed('file) & open('file) |->
    remove(Closed)
}

// It fails on valid3: it only checks of a file is written
// to that is not open. Not whether it is closed. In valid3
// the file is not open, but it has not been closed neither.

class M extends CSVMonitor {
  val open, write, close = event
  val Open = fact

  "r1" -- open('file) |-> insert(Open('file))

  "r2" -- Open('file) & close('file) |->
    remove(Open)

  "r3" -- write('file) & not(Open('file)) |->
    fail("write to closed file")
}

object Evaluate extends MonitorFeeder {
  val open = 'open
  val write = 'write
  val close = 'close

  val trace1 = List(
    open(634540230),
    open(1840976765),
    write(634540230),
    close(634540230))

  val trace2 = List(
    open(634540230),
    write(634540230),
    write(634540230))

  val trace3 = List(
    open(634540230),
    write(1840976765),
    close(634540230))

  val trace_1 = List(
    open(634540230),
    open(1840976765),
    write(634540230),
    close(634540230),
    write(634540230))

  val trace_2 = List(
    open(634540230),
    write(1840976765),
    close(634540230),
    write(634540230))

  val trace_3 = List(
    open(634540230),
    write(634540230),
    write(634540230),
    close(634540230),
    write(634540230))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}
