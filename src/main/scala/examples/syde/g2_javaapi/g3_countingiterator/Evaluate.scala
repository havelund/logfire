package examples.syde.g2_javaapi.g3_countingiterator

import rete._

/**
 * Counting iterator.
 *
 * If a java.util.Iterator object is created from a collec-
 * tion of size s then we can only call next on that iterator at most s times.
 */

// Use this one:

// Use this:

class CountingIterator1 extends Monitor {
  "r1" -- 'iterator('i, 'csize) |-> 'Iterate('i, 'csize)
  "r2" -- 'Iterate('i, 'csize) & 'next('i) |-> {
    if ('csize > 0)
      update('Iterate('i, 'csize - 1))
    else
      fail()
  }
}

object Evaluate1 extends MonitorFeeder {
  val iterator = 'iterator
  val next = 'next

  val set1: Set[Int] = Set(1, 2, 3)
  val set2: Set[String] = Set("A", "B")

  val it1 = set1.iterator
  val it2 = set2.iterator

  val trace1 = List(
    iterator(it1, set1.size),
    iterator(it2, set2.size),
    next(it1),
    next(it2),
    next(it1),
    next(it2),
    next(it1))

  val trace_1 = List(
    iterator(it1, set1.size),
    iterator(it2, set2.size),
    next(it1),
    next(it2),
    next(it1),
    next(it2),
    next(it1),
    next(it2))

  def main(args: Array[String]) {
    val m = new CountingIterator1
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}

class CountingIterator2 extends Monitor {
  "r1" -- 'iterator('c, 'i) |-> {
    val size = get[Iterable[Any]]('c).size
    insert('Iterate('i, size))
  }
  "r2" -- 'Iterate('i, 'size) & 'next('i) |-> {
    if ('size > 0)
      update('Iterate('i, 'size - 1))
    else
      fail()
  }
}

object Evaluate2 extends MonitorFeeder {
  val iterator = 'iterator
  val next = 'next

  val set1: Set[Int] = Set(1, 2, 3)
  val set2: Set[String] = Set("A", "B")

  val it1 = set1.iterator
  val it2 = set2.iterator

  val trace1 = List(
    iterator(set1, it1),
    iterator(set2, it2),
    next(it1),
    next(it2),
    next(it1),
    next(it2),
    next(it1))

  val trace_1 = List(
    iterator(set1, it1),
    iterator(set2, it2),
    next(it1),
    next(it2),
    next(it1),
    next(it2),
    next(it1),
    next(it2))

  def main(args: Array[String]) {
    val m = new CountingIterator2
    m.PRINT = true
    trace1 foreach m.addMapEvent
  }
}