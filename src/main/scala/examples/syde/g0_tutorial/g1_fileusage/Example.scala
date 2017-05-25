package examples.syde.g0_tutorial.g1_fileusage

import rete._

/**
 * File usage.
 *
 * The property concerns the following file usage related events:
 *
 * – open(f, m, b) records file f being opened in mode m (R or W) of size b bytes
 * – close(f) records closing file f
 * – read(f) records reading a file f
 * – write(f,b) records writing b bytes to file f
 * – save(f) records saving file f as version v
 *
 * The correct usage specified by this property can be captured by the following statements:
 *
 * – A file starts closed and cannot be opened (closed) if already open (closed)
 * – A file if opened must eventually be closed
 * – A file can only be read if it is open
 * – A file can only be written to if it is open in write mode
 * – A file cannot be closed if there are unsaved writes – No file can exceed M bytes
 */

class FileUsage extends Monitor {
  "r1" -- 'open('f, 'm, 'size) & not('Open('f, '_, '_)) |-> insert('Open('f, 'm, 'size))
  "r2" -- 'Open('f, '_, '_) & 'open('f, '_, '_) |-> fail()
  "r3" -- 'Open('f, '_, '_) & 'close('f) |-> remove('Open)
  "r4" -- 'Open('f, 'm, '_) & 'read('f) |-> ensure('m.string == "R")
  "r5" -- 'read('f) & not('Open('f, '_, '_)) |-> fail()
  "r6" -- 'Open('f, 'm, 'size) & 'write('f, 'b) |-> {
    ensure('m.string == "W" && 'size + 'b <= 300)
    update('Open('f, 'm, 'size + 'b))
  }
  "r7" -- 'write('f) & not('Open('f, '_, '_)) |-> fail()

  hot('Open)
}

class FileUsageSyntax extends Monitor {
  // This is used to illustrate what rule r4 expands into.
  
  nameToRuleDefinition("r4").--(kindToConditionArguments('Open).apply(('f, 'm, '_))).&(kindToConditionArguments('read).apply('f)).|->{ensure('m.string == "R")}  
}

object Evaluate extends MonitorFeeder {
  val open = 'open
  val close = 'close
  val read = 'read
  val write = 'write

  val trace1 = List(
    open("f1", "R", 200),
    read("f1"),
    open("f2", "W", 100),
    write("f2", 50),
    write("f2", 100),
    write("f2", 30),
    close("f2"),
    read("f1"),
    close("f1"))

  val trace_1 = List(
    open("f1", "R", 200),
    read("f1"),
    open("f2", "W", 100),
    write("f2", 100),
    write("f2", 50),
    write("f2", 60),
    close("f2"),
    read("f1"),
    write("f1", 10))

  val trace_2 = List(
    open("A", "R", 20),
    open("B", "W", 200),
    write("B", 100),
    read("A"),
    write("B", 100),
    close("B"))

  def main(args: Array[String]) {
    val m = new FileUsage
    m.PRINT = true
    trace_2 foreach m.addMapEvent
    m.terminate()
  }
}
