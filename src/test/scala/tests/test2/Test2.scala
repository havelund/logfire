package tests.test2

import rete._
import scala.language.reflectiveCalls
import org.junit.Test

trait CommandMonitor extends Monitor {
  val COMMAND, SUCCESS, RUN, LIMITED = event
  val Commanded = fact
}

// ===================
// --- Monitor M1: ---
// ===================

// Rajeev's layered view of a log.

class M1 extends CommandMonitor {

  // Requirement: two commands should not execute at the same time.
  // This means that runs should not overlap.

  def exclusive(begin1: Int, end1: Int)(begin2: Int, end2: Int) =
    end1 < begin2 || end2 < begin1

  "begin RUN" -- COMMAND('n, 'x, 't) |-> Commanded('x, 't)

  "end RUN" -- Commanded('x, 't1) == 'l & SUCCESS('n, 'x, 't2) |-> (RUN('x, 't1, 't2), rem('l))

  "exclusive" -- RUN('x, 't1, 't2) & RUN('y, 't3, 't4) |-> {
    if ('x.i != 'y.i && !exclusive('t1, 't2)('t3, 't4)) {
      fail("*** overlapping runs %d %d and %d %d: ".format('t1.i, 't2.i, 't3.i, 't4.i))
    }
  }

}

class Test2_1 extends Contract {
  @Test def test() {
    val r = new M1
    setMonitor(r, false)

    r.addEvent('COMMAND)("START_DRIVING", 1, 1000) // 1
    facts(
      'Commanded(1, 1000))

    r.addEvent('SUCCESS)("START_DRIVING", 1, 7000) // 2
    facts(
      'RUN(1, 1000, 7000))

    r.addEvent('COMMAND)("START_RADIO", 2, 10000) // 3
    facts(
      'Commanded(2, 10000),
      'RUN(1, 1000, 7000))

    r.addEvent('SUCCESS)("START_RADIO", 2, 24000) // 4
    facts(
      'RUN(1, 1000, 7000),
      'RUN(2, 10000, 24000))

    r.addEvent('COMMAND)("START_CAMERA", 3, 22000) // 5
    facts(
      'Commanded(3, 22000),
      'RUN(1, 1000, 7000),
      'RUN(2, 10000, 24000))

    r.addEvent('SUCCESS)("START_CAMERA", 3, 40000) // 6
    facts(
      'RUN(3, 22000, 40000),
      'RUN(1, 1000, 7000),
      'RUN(2, 10000, 24000))

    r.addEvent('COMMAND)("STOP_CAMERA", 4, 70000) // 7
    facts(
      'Commanded(4, 70000),
      'RUN(3, 22000, 40000),
      'RUN(1, 1000, 7000),
      'RUN(2, 10000, 24000))

    r.addEvent('SUCCESS)("STOP_CAMERA", 4, 75000) // 8
    facts(
      'RUN(3, 22000, 40000),
      'RUN(4, 70000, 75000),
      'RUN(1, 1000, 7000),
      'RUN(2, 10000, 24000))

    result(
      Report(
        "ERROR *** overlapping runs 22000 40000 and 10000 24000: ",
        (3, "begin RUN", 'Commanded(2, 10000)),
        (4, "end RUN", 'RUN(2, 10000, 24000)),
        (5, "begin RUN", 'Commanded(3, 22000)),
        (6, "exclusive", 'Fail("ERROR *** overlapping runs 22000 40000 and 10000 24000: ")),
        (6, "end RUN", 'RUN(3, 22000, 40000))),
      Report(
        "ERROR *** overlapping runs 10000 24000 and 22000 40000: ",
        (3, "begin RUN", 'Commanded(2, 10000)),
        (4, "end RUN", 'RUN(2, 10000, 24000)),
        (5, "begin RUN", 'Commanded(3, 22000)),
        (6, "end RUN", 'RUN(3, 22000, 40000)),
        (6, "exclusive", 'Fail("ERROR *** overlapping runs 10000 24000 and 22000 40000: "))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M2: ---
// ===================

// Illustrating different ways to add facts to and remove facts from
// a monitor.

trait Intervals {
  def intersection(begin1: Int, end1: Int)(begin2: Int, end2: Int): (Int, Int) = {
    val i0 = begin1.max(begin2) // lower bound of intersection interval
    val i1 = end1.min(end2) // upper bound of intersection interval
    (i0, i1)
  }

  def overlap(intervals: (Int, Int)*): Boolean = {
    val (x, y) :: rest = intervals.toList
    var (left, right) = (x, y)
    for ((x, y) <- rest) {
      val (left_, right_) = intersection(left, right)(x, y)
      if (left_ < right_) {
        left = left_
        right = right_
      } else {
        return false
      }
    }
    true
  }
}

class M2 extends CommandMonitor with Intervals {

  // Requirement: two commands should not execute at the same time when the space craft is
  // in resource limited mode. This means that runs should not overlap.

  "begin RUN " -- COMMAND('n, 'x, 't) |-> Commanded('x, 't)
  "end RUN" -- Commanded('x, 't1) == 'l & SUCCESS('n, 'x, 't2) |-> (RUN('x, 't1, 't2), rem('l))

  "exclusive" -- RUN('x, 't1, 't2) & RUN('y, 't3, 't4) & LIMITED('t5, 't6) |-> {
    if ('x.i != 'y.i && overlap(('t1, 't2), ('t3, 't4), ('t5, 't6))) {
      fail("*** overlapping runs %d[%d-%d] and %d[%d %d] when limited [%d-%d]".format(
        'x.i, 't1.i, 't2.i, 'y.i, 't3.i, 't4.i, 't5.i, 't6.i))
    }
  }

}

class Test2_2 extends Contract {
  @Test def test() {
    val r = new M2
    setMonitor(r, false)

    val limited1 = r.addFact('LIMITED)(500, 1500)
    facts(
      'LIMITED(500, 1500))

    val limited2 = r.addFact('LIMITED)(17000, 18000)
    facts(
      'LIMITED(500, 1500),
      'LIMITED(17000, 18000))

    val limited3 = r.addFact('LIMITED)(3000, 5000)
    facts(
      'LIMITED(500, 1500),
      'LIMITED(3000, 5000),
      'LIMITED(17000, 18000))

    r.addEvent('COMMAND)("START_DRIVING", 1, 1000) // 1
    facts(
      'LIMITED(500, 1500),
      'LIMITED(3000, 5000),
      'LIMITED(17000, 18000),
      'Commanded(1, 1000))

    r.addEvent('SUCCESS)("START_DRIVING", 1, 5000) // 2
    facts(
      'LIMITED(500, 1500),
      'LIMITED(3000, 5000),
      'LIMITED(17000, 18000),
      'RUN(1, 1000, 5000))

    limited3.remove()
    facts(
      'LIMITED(500, 1500),
      'LIMITED(17000, 18000),
      'RUN(1, 1000, 5000))

    r.addEvent('COMMAND)("START_RADIO", 2, 3000) // 3
    facts(
      'LIMITED(500, 1500),
      'LIMITED(17000, 18000),
      'RUN(1, 1000, 5000),
      'Commanded(2, 3000))

    r.addEvent('SUCCESS)("START_RADIO", 2, 8000) // 4
    facts(
      'LIMITED(500, 1500),
      'LIMITED(17000, 18000),
      'RUN(2, 3000, 8000),
      'RUN(1, 1000, 5000))

    r.addEvent('COMMAND)("START_CAMERA", 3, 10000) // 5
    facts(
      'RUN(2, 3000, 8000),
      'Commanded(3, 10000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'LIMITED(500, 1500))

    r.addEvent('SUCCESS)("START_CAMERA", 3, 20000) // 6
    facts(
      'RUN(2, 3000, 8000),
      'RUN(3, 10000, 20000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'LIMITED(500, 1500))

    r.addEvent('COMMAND)("START_DRILL", 4, 15000) // 7
    facts(
      'RUN(2, 3000, 8000),
      'RUN(3, 10000, 20000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'Commanded(4, 15000),
      'LIMITED(500, 1500))

    r.addEvent('SUCCESS)("START_DRILL", 4, 25000) // 8
    facts(
      'RUN(2, 3000, 8000),
      'RUN(3, 10000, 20000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'RUN(4, 15000, 25000),
      'LIMITED(500, 1500))

    r.addMapFact(Map('kind -> 'LIMITED, 'one -> 3000, 'two -> 5000))
    facts(
      'RUN(2, 3000, 8000),
      'LIMITED(3000, 5000),
      'RUN(3, 10000, 20000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'RUN(4, 15000, 25000),
      'LIMITED(500, 1500))

    r.addMapFact('kind -> 'LIMITED, 'one -> 3000, 'two -> 5000)
    facts(
      'RUN(2, 3000, 8000),
      'LIMITED(3000, 5000),
      'RUN(3, 10000, 20000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'RUN(4, 15000, 25000),
      'LIMITED(500, 1500))

    r.addMapFact('LIMITED)('one -> 3000, 'two -> 5000)
    facts(
      'RUN(2, 3000, 8000),
      'LIMITED(3000, 5000),
      'RUN(3, 10000, 20000),
      'RUN(1, 1000, 5000),
      'LIMITED(17000, 18000),
      'RUN(4, 15000, 25000),
      'LIMITED(500, 1500))

    result(
      Report(
        "ERROR *** overlapping runs 2[3000-8000] and 1[1000 5000] when limited [3000-5000]",
        (1, "begin RUN ", 'Commanded(1, 1000)),
        (2, "end RUN", 'RUN(1, 1000, 5000)),
        (3, "begin RUN ", 'Commanded(2, 3000)),
        (4, "end RUN", 'RUN(2, 3000, 8000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 2[3000-8000] and 1[1000 5000] when limited [3000-5000]"))),
      Report(
        "ERROR *** overlapping runs 1[1000-5000] and 2[3000 8000] when limited [3000-5000]",
        (1, "begin RUN ", 'Commanded(1, 1000)),
        (2, "end RUN", 'RUN(1, 1000, 5000)),
        (3, "begin RUN ", 'Commanded(2, 3000)),
        (4, "end RUN", 'RUN(2, 3000, 8000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 1[1000-5000] and 2[3000 8000] when limited [3000-5000]"))),
      Report(
        "ERROR *** overlapping runs 2[3000-8000] and 1[1000 5000] when limited [3000-5000]",
        (1, "begin RUN ", 'Commanded(1, 1000)),
        (2, "end RUN", 'RUN(1, 1000, 5000)),
        (3, "begin RUN ", 'Commanded(2, 3000)),
        (4, "end RUN", 'RUN(2, 3000, 8000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 2[3000-8000] and 1[1000 5000] when limited [3000-5000]"))),
      Report(
        "ERROR *** overlapping runs 1[1000-5000] and 2[3000 8000] when limited [3000-5000]",
        (1, "begin RUN ", 'Commanded(1, 1000)),
        (2, "end RUN", 'RUN(1, 1000, 5000)),
        (3, "begin RUN ", 'Commanded(2, 3000)),
        (4, "end RUN", 'RUN(2, 3000, 8000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 1[1000-5000] and 2[3000 8000] when limited [3000-5000]"))),
      Report(
        "ERROR *** overlapping runs 2[3000-8000] and 1[1000 5000] when limited [3000-5000]",
        (1, "begin RUN ", 'Commanded(1, 1000)),
        (2, "end RUN", 'RUN(1, 1000, 5000)),
        (3, "begin RUN ", 'Commanded(2, 3000)),
        (4, "end RUN", 'RUN(2, 3000, 8000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 2[3000-8000] and 1[1000 5000] when limited [3000-5000]"))),
      Report(
        "ERROR *** overlapping runs 1[1000-5000] and 2[3000 8000] when limited [3000-5000]",
        (1, "begin RUN ", 'Commanded(1, 1000)),
        (2, "end RUN", 'RUN(1, 1000, 5000)),
        (3, "begin RUN ", 'Commanded(2, 3000)),
        (4, "end RUN", 'RUN(2, 3000, 8000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 1[1000-5000] and 2[3000 8000] when limited [3000-5000]"))),
      Report(
        "ERROR *** overlapping runs 4[15000-25000] and 3[10000 20000] when limited [17000-18000]",
        (5, "begin RUN ", 'Commanded(3, 10000)),
        (6, "end RUN", 'RUN(3, 10000, 20000)),
        (7, "begin RUN ", 'Commanded(4, 15000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 4[15000-25000] and 3[10000 20000] when limited [17000-18000]")),
        (8, "end RUN", 'RUN(4, 15000, 25000))),
      Report(
        "ERROR *** overlapping runs 3[10000-20000] and 4[15000 25000] when limited [17000-18000]",
        (5, "begin RUN ", 'Commanded(3, 10000)),
        (6, "end RUN", 'RUN(3, 10000, 20000)),
        (7, "begin RUN ", 'Commanded(4, 15000)),
        (8, "end RUN", 'RUN(4, 15000, 25000)),
        (8, "exclusive", 'Fail("ERROR *** overlapping runs 3[10000-20000] and 4[15000 25000] when limited [17000-18000]"))))

    //r.draw("test/output/scalashow.dot")
  }
}
