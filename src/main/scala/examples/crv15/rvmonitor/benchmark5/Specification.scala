package examples.crv15.rvmonitor.benchmark5

import rete._

/*
 * This property specifies a common requirement in concurrent programming: 
 * a thread should release a lock as many times as it acquires the lock; 
 * otherwise it may cause deadlock and the program may not terminate. When 
 * monitoring log files against this property, rv-monitor will report violation 
 * if it finds a thread acquires a lock m times, but releases that lock a different 
 * number of times. 
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  // event, ALock, AThread

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    val kind = Symbol(record.get(0))
    map += 'kind -> kind
    kind match {
      // begin, , thread
      case 'begin =>
        map += 'one -> record.get(2).trim
      // acquire, lock, thread
      case 'acquire =>
        map += 'one -> record.get(1).trim
        map += 'two -> record.get(2).trim
      // release, lock, thread
      case 'release =>
        map += 'one -> record.get(1).trim
        map += 'two -> record.get(2).trim
      // end, , thread
      case 'end =>
        map += 'one -> record.get(2).trim        
    }
    addMapEvent(map)
  }
}

class M extends CSVMonitor {
  val begin, end, acquire, release = event
  val Inside, Locked = fact

  // Calls:

  "r1" -- begin('t) & not(Inside('t, '_)) |-> insert(Inside('t, 1))

  "r2" -- Inside('t, 'd) & begin('t) |-> update(Inside('t, 'd + 1))

  "r3" -- Inside('t, 'd) & end('t) |-> {
    if ('d > 1)
      update(Inside('t, 'd - 1))
    else
      remove(Inside)
  }

  // Correct acquisitions and releases:

  "r4" -- Inside('t, 'd) & acquire('l, 't) & not(Locked('l, 't, 'd, '_)) |-> insert(Locked('l, 't, 'd, 1))

  "r5" -- Inside('t, 'd) & Locked('l, 't, 'd, 'c) & acquire('l, 't) |-> update(Locked('l, 't, 'd, 'c + 1))

  "r6" -- Inside('t, 'd) & Locked('l, 't, 'd, 'c) & release('l, 't) |-> {
    if ('c > 1)
      update(Locked('l, 't, 'd, 'c - 1))
    else
      remove(Locked)
  }

  "r7" -- acquire('l, 't) & not(Inside('t, '_)) |-> {
    insert(Inside('t, 0))
    insert(Locked('l, 't, 0, 1))
  }

  // Failures:

  "r8" -- Inside('t, 'd) & release('l, 't) & not(Locked('l, 't, 'd, '_)) |->
    fail("release of lock not acquired within current method invocation")

  "r9" -- Inside('t, 'd) & Locked('_, 't, 'd, '_) & end('t) |->
    fail("return from method before all locks acquired in that method are released")

  "r10" -- release('l, 't) & not(Locked('l, 't, '_, '_)) |->
    fail("release of lock not acquired")

  "r11" -- END() & Inside('t, 'c) |-> {
    avoid("method call that is not returned from")('c > 0)
  }

  hot(Locked)
}

object Evaluate extends MonitorFeeder {
  val begin = 'begin
  val end = 'end
  val acquire = 'acquire
  val release = 'release

  val trace1 = List(
    begin(148912029),
    acquire(874217650, 148912029),
    acquire(874217650, 148912029),
    release(874217650, 148912029),
    release(874217650, 148912029),
    end(148912029))

  val trace2 = List(
    acquire(874217650, 148912029),
    release(874217650, 148912029))

  val trace3 = List(
    acquire(874217650, 148912029),
    begin(1564984895),
    acquire(1587819720, 1564984895),
    release(1587819720, 1564984895),
    end(1564984895),
    release(874217650, 148912029))

  val trace_1 = List(
    acquire(874217650, 148912029),
    acquire(874217650, 148912029),
    release(874217650, 148912029),
    release(874217650, 148912029),
    release(874217650, 148912029))

  val trace_2 = List(
    acquire(874217650, 148912029),
    acquire(874217650, 148912029),
    release(874217650, 148912029))

  val trace_3 = List(
    begin(148912029),
    acquire(874217650, 148912029),
    end(148912029),
    release(874217650, 148912029))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace1 foreach m.addMapEvent
    m.terminate()
  }
}
  