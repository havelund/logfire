package examples.syde.g4_concurrency.g2_locknesting

import rete._

/**
 * Lock nesting.
 *
 * A thread should release a lock as many times as it acquires the lock. Otherwise
 * it may cause deadlock and the program may not terminate. Additionally, locks
 * taken within a call to a method should be released during that call.
 *
 * DISCUSION:
 *
 * What about the second part of this property? Giles has not modelled it.
 */

class LockNesting extends Monitor {
  // Calls:
  "r1" -- 'begin('t) & not('Inside('t, '_)) |-> insert('Inside('t, 1))
  "r2" -- 'Inside('t, 'd) & 'begin('t) |-> update('Inside('t, 'd + 1))
  "r3" -- 'Inside('t, 'd) & 'end('t) |-> {
    if ('d.int > 1)
      update('Inside('t, 'd - 1))
    else
      remove('Inside)
  }

  // Locks:
  "r4" -- 'Inside('t, 'd) & 'lock('t, 'l) & not('Locked('t, 'l, 'd, '_)) |-> insert('Locked('t, 'l, 'd, 1))
  "r5" -- 'Inside('t, 'd) & 'Locked('t, 'l, 'd, 'c) & 'lock('t, 'l) |-> update('Locked('t, 'l, 'd, 'c + 1))
  "r6" -- 'Inside('t, 'd) & 'Locked('t, 'l, 'd, 'c) & 'unlock('t, 'l) |-> {
    if ('c > 1)
      update('Locked('t, 'l, 'd, 'c - 1))
    else
      remove('Locked)
  }
  "r7" -- 'Inside('t, 'd) & 'unlock('t, 'l) & not('Locked('t, 'l, 'd, '_)) |-> fail()
  "r8" -- 'Inside('t, 'd) & 'Locked('t, '_, 'd, '_) & 'end('t) |-> fail()

  hot('Locked)
}

object Evaluate extends MonitorFeeder {
  val begin = 'begin
  val end = 'end
  val lock = 'lock
  val unlock = 'unlock

  val trace1 = List(
    begin("T1"),
    lock("T1", "l1"),
    lock("T1", "l1"),
    lock("T1", "l2"),
    lock("T1", "l2"),
    unlock("T1", "l2"),
    begin("T1"),
    lock("T1", "l1"),
    lock("T1", "l1"),
    unlock("T1", "l1"),
    unlock("T1", "l1"),
    end("T1"),
    unlock("T1", "l1"),
    unlock("T1", "l1"),
    unlock("T1", "l2"),
    end("T1"))

  val trace_1 = List(
    begin("T1"),
    lock("T1", "l1"),
    lock("T1", "l1"),
    lock("T1", "l2"),
    lock("T1", "l2"),
    unlock("T1", "l2"),
    begin("T1"),
    lock("T1", "l1"),
    lock("T1", "l1"),
    //unlock("T1", "l1"),
    unlock("T1", "l1"),
    end("T1"),
    unlock("T1", "l1"),
    unlock("T1", "l1"),
    unlock("T1", "l2"),
    end("T1"))

  def main(args: Array[String]) {
    val m = new LockNesting
    m.PRINT = true
    trace_1 foreach m.addMapEvent
    m.terminate()
  }
}
