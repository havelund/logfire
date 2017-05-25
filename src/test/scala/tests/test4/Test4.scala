package tests.test4

import rete._
import scala.language.reflectiveCalls
import org.junit.Test

trait EventMonitor extends Monitor {
  val EVR, EHA = event
}

// Testing don't care: '_.
// This spec makes no sense whatsoever.

class M1 extends EventMonitor {
  val A, B = fact

  "r0" -- EVR(1, '_) |-> A(0)

  "r1" -- EVR('_, 'x) & A('_) |-> update(A('x))

  "r2" -- EVR('_, '_) & A('_) |-> insert(B('_4, '_5, '_6))

  "r3" -- END() & B('x, '_, '_) |-> fail(s"${'x.i}")

}

class Test4_1 extends Contract {
  @Test def test() {
    val m = new M1
    setMonitor(m, false)

    m.addEvent('EVR)(1, 2) // 1
    facts(
      'A(0))

    m.addEvent('EVR)(2, 3) // 2
    facts(
      'A(3),
      'B(2, 3, 0))

    m.addEvent('EVR)(3, 4) // 3
    facts(
      'A(4),
      'B(2, 3, 0),
      'B(3, 4, 3))

    m.addEvent('END)() // 4
    facts(
      'A(4),
      'B(2, 3, 0),
      'B(3, 4, 3))

    result(
      Report(
        "ERROR 3",
        (1, "r0", 'A(0)),
        (2, "r1", 'A(3)),
        (3, "r2", 'B(3, 4, 3)),
        (4, "r3", 'Fail("ERROR 3"))),
      Report(
        "ERROR 2",
        (1, "r0", 'A(0)),
        (2, "r2", 'B(2, 3, 0)),
        (4, "r3", 'Fail("ERROR 2"))))
  }
}

// Testing 'x_i, 'x.f and 'x.s
// Testing insert, remove, update and ensure

class M2 extends EventMonitor {
  val A, B, C, D = fact

  "r1" -- A('c) == 'l & EVR('x) |-> {
    ensure(s"counter A(${'c.i}) should be less than two")('c < 2)
    remove('l)
    insert(A('c.i + 'x.i))
  }

  "r2" -- EVR('x) & B('c) |-> {
    ensure(s"counter B(${'c.i}) should be less than two")('c < 2)
    remove(B)
    insert(B('c.i + 'x.i))
  }

  "r3" -- EVR('x) & C('c) |-> {
    ensure(s"counter C(${'c.i}) should be less than two")('c < 2)
    update(C('c.i + 1))
  }

  "r4" -- EVR('x) & D('c) |-> ensure(s"counter D(${'c.i}) should be less than two")('c < 2)

  "r5" -- EVR('x) & D('c) |-> update(D('c.i + 1))

  addFact(A)(0)
  addFact(B)(0)
  addFact(C)(0)
  addFact(D)(0)
}

class Test4_2 extends Contract {
  @Test def test() {
    val m = new M2
    setMonitor(m, false)

    facts(
      'A(0),
      'C(0),
      'B(0),
      'D(0))

    m.addEvent('EVR)(1) // 1
    facts(
      'A(1),
      'C(1),
      'B(1),
      'D(1))

    m.addEvent('EVR)(1) // 2
    facts(
      'A(2),
      'C(2),
      'B(2),
      'D(2))

    m.addEvent('EVR)(1) // 3
    facts(
      'A(3),
      'C(3),
      'B(3),
      'D(3))

    m.addEvent('END)() // 4
    facts(
      'A(3),
      'C(3),
      'B(3),
      'D(3))

    result(
      Report(
        "ERROR [counter A(2) should be less than two] : failed!",
        (1, "r1", 'A(1)),
        (2, "r1", 'A(2)),
        (3, "r1", 'Fail("ERROR [counter A(2) should be less than two] : failed!"))),
      Report(
        "ERROR [counter B(2) should be less than two] : failed!",
        (1, "r2", 'B(1)),
        (2, "r2", 'B(2)),
        (3, "r2", 'Fail("ERROR [counter B(2) should be less than two] : failed!"))),
      Report(
        "ERROR [counter C(2) should be less than two] : failed!",
        (1, "r3", 'C(1)),
        (2, "r3", 'C(2)),
        (3, "r3", 'Fail("ERROR [counter C(2) should be less than two] : failed!"))),
      Report(
        "ERROR [counter D(2) should be less than two] : failed!",
        (1, "r5", 'D(1)),
        (2, "r5", 'D(2)),
        (3, "r4", 'Fail("ERROR [counter D(2) should be less than two] : failed!"))))
  }
}

// Testing clearFacts, writeFacts, readFacts and getFacts

class M3 extends EventMonitor {
  val Started, Phase = fact

  "r1" -- EVR('command -> "START", 'name -> 'n, 'time -> 't1) |-> Started('n, 't1)

  "r2" -- EVR('command -> "STOP", 'name -> 'n, 'time -> 't2) & Started('n, 't1) |-> Phase('n, 't1, 't2)

  "r3" -- Phase('n1, 't1, 't2) & Phase('n2, 't3, 't4) |->
    ensure(s"phrases should not overlap ${'t1.i}-${'t2.i}:${'t3.i}-${'t4.i}")(
      'n1.s == 'n2.s || 't2.i < 't3.i || 't4.i < 't1.i)

}

class Test4_3 extends Contract {
  @Test def test() {
    val m = new M3
    setMonitor(m, false)

    m.addMapEvent('EVR)('command -> "START", 'name -> "DRIVE", 'time -> 1000) // 1
    facts(
      'Started("DRIVE", 1000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "DRIVE", 'time -> 2000) // 2
    facts(
      'Phase("DRIVE", 1000, 2000),
      'Started("DRIVE", 1000))

    m.addMapEvent('EVR)('command -> "START", 'name -> "CALLHOME", 'time -> 3000) // 3
    facts(
      'Phase("DRIVE", 1000, 2000),
      'Started("DRIVE", 1000),
      'Started("CALLHOME", 3000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "CALLHOME", 'time -> 4000) // 4
    facts(
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000),
      'Started("DRIVE", 1000),
      'Started("CALLHOME", 3000))

    m.addMapEvent('EVR)('command -> "START", 'name -> "TAKEPIC", 'time -> 5000) // 5
    facts(
      'Started("DRIVE", 1000),
      'Phase("DRIVE", 1000, 2000),
      'Started("TAKEPIC", 5000),
      'Started("CALLHOME", 3000),
      'Phase("CALLHOME", 3000, 4000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "TAKEPIC", 'time -> 6000) // 6
    facts(
      'Started("DRIVE", 1000),
      'Phase("DRIVE", 1000, 2000),
      'Started("TAKEPIC", 5000),
      'Started("CALLHOME", 3000),
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("CALLHOME", 3000, 4000))

    val file1 = "/Users/khavelun/Desktop/SCALADB1.ser"
    val file2 = "/Users/khavelun/Desktop/SCALADB2.ser"

    m.writeFacts(file1)
    m.writeFacts(file2, _('kind) == 'Phase)

    m.clearFacts(_('kind) == 'Started)
    facts(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    val expected1 = Set(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))
    assert(m.getMaps() == expected1)

    m.clearFacts()
    facts()

    val expected2 = Set(
      'Started("DRIVE", 1000),
      'Phase("DRIVE", 1000, 2000),
      'Started("TAKEPIC", 5000),
      'Started("CALLHOME", 3000),
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("CALLHOME", 3000, 4000))
    assert(m.getFacts(file1) == expected2)

    val expected3 = Set(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))
    assert(m.getFacts(file2) == expected3)

    m.readFacts(file1, _('kind) == 'Phase)
    facts(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    m.clearFacts()
    facts()

    m.readFacts(file2)
    facts(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    m.addMapEvent('EVR)('command -> "START", 'name -> "TURN", 'time -> 3500) // 7
    facts(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000),
      'Started("TURN", 3500))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "TURN", 'time -> 5500) // 8
    facts(
      'Started("TURN", 3500),
      'Phase("DRIVE", 1000, 2000),
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("CALLHOME", 3000, 4000),
      'Phase("TURN", 3500, 5500))

    // The error traces provided are missing information due to facts being added via reloading.

    result(
      Report(
        "ERROR [phrases should not overlap 3500-5500:3000-4000] : failed!",
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r3", 'Fail("ERROR [phrases should not overlap 3500-5500:3000-4000] : failed!")),
        (8, "r2", 'Phase("TURN", 3500, 5500))),
      Report(
        "ERROR [phrases should not overlap 3500-5500:5000-6000] : failed!",
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r3", 'Fail("ERROR [phrases should not overlap 3500-5500:5000-6000] : failed!")),
        (8, "r2", 'Phase("TURN", 3500, 5500))),
      Report(
        "ERROR [phrases should not overlap 3000-4000:3500-5500] : failed!",
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r3", 'Fail("ERROR [phrases should not overlap 3000-4000:3500-5500] : failed!")),
        (8, "r2", 'Phase("TURN", 3500, 5500))),
      Report(
        "ERROR [phrases should not overlap 5000-6000:3500-5500] : failed!",
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r3", 'Fail("ERROR [phrases should not overlap 5000-6000:3500-5500] : failed!")),
        (8, "r2", 'Phase("TURN", 3500, 5500))))
  }
}

// Testing replace

class M4 extends EventMonitor {
  val Started, Phase = fact

  "r1" -- EVR('command -> "START", 'name -> 'n, 'time -> 't1) |-> Started('n, 't1)

  "r2" -- EVR('command -> "STOP", 'name -> 'n, 'time -> 't2) & Started('n, 't1) |-> replace(Started)(Phase('n, 't1, 't2))

  "r3" -- Phase('n1, 't1, 't2) & Phase('n2, 't3, 't4) |->
    ensure(s"phases should not overlap ${'t1.i}-${'t2.i}:${'t3.i}-${'t4.i}")('n1.s == 'n2.s || 't2.i < 't3.i || 't4.i < 't1.i)

}

class Test4_4 extends Contract {
  @Test def test() {
    val m = new M4
    setMonitor(m, false)

    m.addMapEvent('EVR)('command -> "START", 'name -> "DRIVE", 'time -> 1000) // 1
    facts(
      'Started("DRIVE", 1000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "DRIVE", 'time -> 2000) // 2
    facts(
      'Phase("DRIVE", 1000, 2000))

    m.addMapEvent('EVR)('command -> "START", 'name -> "CALLHOME", 'time -> 3000) // 3
    facts(
      'Started("CALLHOME", 3000),
      'Phase("DRIVE", 1000, 2000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "CALLHOME", 'time -> 4000) // 4
    facts(
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    m.addMapEvent('EVR)('command -> "START", 'name -> "TAKEPIC", 'time -> 5000) // 5
    facts(
      'Started("TAKEPIC", 5000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "TAKEPIC", 'time -> 6000) // 6
    facts(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    m.addMapEvent('EVR)('command -> "START", 'name -> "TURN", 'time -> 3500) // 7
    facts(
      'Started("TURN", 3500),
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("CALLHOME", 3000, 4000))

    m.addMapEvent('EVR)('command -> "STOP", 'name -> "TURN", 'time -> 5500) // 8
    facts(
      'Phase("TAKEPIC", 5000, 6000),
      'Phase("DRIVE", 1000, 2000),
      'Phase("TURN", 3500, 5500),
      'Phase("CALLHOME", 3000, 4000))

    result(
      Report(
        "ERROR [phases should not overlap 3500-5500:3000-4000] : failed!",
        (3, "r1", 'Started("CALLHOME", 3000)),
        (4, "r2", 'Phase("CALLHOME", 3000, 4000)),
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r3", 'Fail("ERROR [phases should not overlap 3500-5500:3000-4000] : failed!")),
        (8, "r2", 'Phase("TURN", 3500, 5500))),
      Report(
        "ERROR [phases should not overlap 3500-5500:5000-6000] : failed!",
        (5, "r1", 'Started("TAKEPIC", 5000)),
        (6, "r2", 'Phase("TAKEPIC", 5000, 6000)),
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r3", 'Fail("ERROR [phases should not overlap 3500-5500:5000-6000] : failed!")),
        (8, "r2", 'Phase("TURN", 3500, 5500))),
      Report(
        "ERROR [phases should not overlap 3000-4000:3500-5500] : failed!",
        (3, "r1", 'Started("CALLHOME", 3000)),
        (4, "r2", 'Phase("CALLHOME", 3000, 4000)),
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r2", 'Phase("TURN", 3500, 5500)),
        (8, "r3", 'Fail("ERROR [phases should not overlap 3000-4000:3500-5500] : failed!"))),
      Report(
        "ERROR [phases should not overlap 5000-6000:3500-5500] : failed!",
        (5, "r1", 'Started("TAKEPIC", 5000)),
        (6, "r2", 'Phase("TAKEPIC", 5000, 6000)),
        (7, "r1", 'Started("TURN", 3500)),
        (8, "r2", 'Phase("TURN", 3500, 5500)),
        (8, "r3", 'Fail("ERROR [phases should not overlap 5000-6000:3500-5500] : failed!"))))
  }
}

// Testing 'ANY

class M5 extends EventMonitor {
  val Occurred = fact

  var counter = 0

  "r1" -- ANY('f -> 'x) |->
    insert(Occurred(s"r1[$counter] with ${'x.i}"))

  "r2" -- ANY('h -> 'x) |->
    insert(Occurred(s"r2[$counter] with ${'x.i}"))

  "r3" -- EVR('three -> 'x) |->
    insert(Occurred(s"r3[$counter] with 'x = ${'x.i}"))

  "r4" -- ANY('f -> 'x, 'g -> 'y) |->
    insert(Occurred(s"r4[$counter] with ('x,'y) = (${'x.i},${'y.i}})"))

  "r5" -- ANY('f -> 'x, 'g -> 'y', 'h -> 'z) |->
    insert(Occurred(s"r5[$counter] with ('x,'y,'z) = (${'x.i},${'y.i},${'z.i})"))

  "r6" -- END() |->
    insert(Occurred(s"r6[$counter]"))
}

class Test4_5 extends Contract {
  @Test def test() {
    val m = new M5
    setMonitor(m, false)

    m.counter += 1
    add('EVR('f -> 1, 'g -> 2))
    delta(
      'Occurred("r1[1] with 1"),
      'Occurred("r4[1] with ('x,'y) = (1,2})"))()

    m.counter += 1
    add('EHA('f -> 3, 'g -> 4))
    delta(
      'Occurred("r1[2] with 3"),
      'Occurred("r4[2] with ('x,'y) = (3,4})"))()

    m.counter += 1
    add('EVR('f -> 5, 'g -> 6))
    delta(
      'Occurred("r4[3] with ('x,'y) = (5,6})"),
      'Occurred("r1[3] with 5"))()

    m.counter += 1
    add('EHA('f -> 7, 'g -> 8))
    delta(
      'Occurred("r1[4] with 7"),
      'Occurred("r4[4] with ('x,'y) = (7,8})"))()

    m.counter += 1
    add('END())
    delta(
      'Occurred("r6[5]"))()
  }
}
