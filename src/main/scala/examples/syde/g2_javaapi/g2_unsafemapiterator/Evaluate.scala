package examples.syde.g2_javaapi.g2_unsafemapiterator

import rete._

/**
 * Unsafe map iterator.
 *
 * If a collection is created from a java.util.Map object
 * (via calls to values or keySet) and then an java.util.Iterator object is
 * created from that collection, then the iterator cannot be used after the original
 * map has been updated.
 */

class UnsafeMapIterator extends Monitor {
  "r1" -- 'create('m, 'c) |-> insert('HasCol('m, 'c))
  "r2" -- 'HasCol('m, 'c) & 'iterator('c, 'i) |-> insert('HasIt('m, 'i))
  "r3" -- 'HasIt('m, 'i) & 'update('m) |-> insert('UpdateM('i))
  "r4" -- 'UpdateM('i) & 'use('i) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val create = 'create
  val iterator = 'iterator
  val updatemap = 'update
  val use = 'use

  val trace1 = List(
    create("m1", "c1_1"),
    create("m1", "c1_2"),
    create("m2", "c2"),
    iterator("c1_1", "it1_1"),
    iterator("c1_2", "it1_2"),
    iterator("c2", "it2"),
    use("it1_1"),
    use("it1_2"),
    use("it2"),
    updatemap("m1"),
    use("it2"))

  val trace_1 = List(
    create("m1", "c1_1"),
    create("m1", "c1_2"),
    create("m2", "c2"),
    iterator("c1_1", "it1_1"),
    iterator("c1_2", "it1_2"),
    iterator("c2", "it2"),
    use("it1_1"),
    use("it1_2"),
    use("it2"),
    updatemap("m1"),
    use("it1_1"),
    use("it2"))

  def main(args: Array[String]) {
    val m = new UnsafeMapIterator
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
