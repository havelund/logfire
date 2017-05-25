package tests.test9

/*
 * Problem: events with positional arguments get generated with formal parameters one, two, ... 
 * Solution: submit map events with the right formal parameters name, number, etc. Hence only deal with map events.
 * Discussion: 
 * - it is not reasonable to work with one, two, etc. Let's get away from that.
 * - hence we should submit events in map format.  
 */

import rete._
//import scala.language.reflectiveCalls
import org.junit.Test

// ===================
// --- Monitor M0: ---
// ===================

class M0 extends Monitor {
  type STRING = +[Int]
  type INT = +[Int]

  // Events:

  case class COMMAND(one: STRING, two: INT) extends Obs
  case class SUCCESS(one: +[String], two: +[Int]) extends Obs
  case class FAIL(one: +[String], two: +[Int]) extends Obs
  case class End() extends Obs

  // Facts:

  case class Commanded(one: +[String], two: +[Int]) extends Obs
  case class Succeeded(one: +[String], two: +[Int]) extends Obs

  // r1: A command should succeed without failing first.

  "r1.1" -- COMMAND('n, 'x) |-> insert(Commanded('n, 'x))
  "r1.2" -- Commanded('n, 'x) == 'cmd & SUCCESS('n, 'x) |-> rem('cmd)
  "r1.3" -- Commanded('n, 'x) == 'cmd & FAIL('n, 'x) |-> (rem('cmd), error("command failed before success"))
  "r1.4" -- END() & Commanded('n, 'x) |-> error("command not succeeded")

  // r2: command should succeed at most once

  "r2.1" -- SUCCESS('n, 'x) & not(Succeeded('n, 'x)) |-> insert(Succeeded('n, 'x))
  "r2.2" -- SUCCESS('n, 'x) & Succeeded('n, 'x) |-> error("two successes")
}

class Test1_0 extends Contract {
  @Test def test() {
    val r = new M0
    setMonitor(r, false)

    add('COMMAND("STOP_DRIVING", 1)) // 1
    facts(
      'Commanded("STOP_DRIVING", 1))

    add('COMMAND("START_CAMERA", 2)) // 2
    facts(
      'Commanded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 1))

    add('COMMAND("TURN_ANTENNA", 3)) // 3
    facts(
      'Commanded("START_CAMERA", 2),
      'Commanded("TURN_ANTENNA", 3),
      'Commanded("STOP_DRIVING", 1))

    add('FAIL("STOP_DRIVING", 1)) // 4
    facts(
      'Commanded("START_CAMERA", 2),
      'Commanded("TURN_ANTENNA", 3))

    add('SUCCESS("START_CAMERA", 2)) // 5
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded("TURN_ANTENNA", 3))

    add('COMMAND("STOP_DRIVING", 4)) // 6
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 4),
      'Commanded("TURN_ANTENNA", 3))

    add('SUCCESS("START_CAMERA", 2)) // 7
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 4),
      'Commanded("TURN_ANTENNA", 3))

    add('END()) // 8
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 4),
      'Commanded("TURN_ANTENNA", 3))

    result(
      Report(
        "ERROR command not succeeded",
        (3, "r1.1", 'Commanded("TURN_ANTENNA", 3)),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR command not succeeded",
        (6, "r1.1", 'Commanded("STOP_DRIVING", 4)),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR two successes",
        (5, "r2.1", 'Succeeded("START_CAMERA", 2)),
        (7, "r2.2", 'Fail("ERROR two successes"))),
      Report(
        "ERROR command failed before success",
        (1, "r1.1", 'Commanded("STOP_DRIVING", 1)),
        (4, "r1.3", 'Fail("ERROR command failed before success"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ==================================
// --- Defining events and facts: ---
// ==================================

// ===================
// --- Monitor M1: ---
// ===================

class M1 extends Monitor {
  // Events:

  case class COMMAND(name: +[String], number: +[Int]) extends Obs
  case class SUCCESS(name: +[String], number: +[Int]) extends Obs
  case class FAIL(name: +[String], number: +[Int]) extends Obs
  case class End() extends Obs

  // Facts:

  case class Commanded(name: +[String], number: +[Int]) extends Obs
  case class Succeeded(name: +[String], number: +[Int]) extends Obs

  // r1: A command should succeed without failing first.

  "r1.1" -- COMMAND('n, 'x) |-> insert(Commanded('n, 'x))
  "r1.2" -- Commanded('n, 'x) == 'cmd & SUCCESS('n, 'x) |-> rem('cmd)
  "r1.3" -- Commanded('n, 'x) == 'cmd & FAIL('n, 'x) |-> (rem('cmd), error("command failed before success"))
  "r1.4" -- End() & Commanded('n, 'x) |-> error("command not succeeded")

  // r2: command should succeed at most once

  "r2.1" -- SUCCESS('n, 'x) & not(Succeeded('n, 'x)) |-> insert(Succeeded('n, 'x))
  "r2.2" -- SUCCESS('n, 'x) & Succeeded('n, 'x) |-> error("two successes")
}

class Test1_1 extends Contract {
  case class COMMAND(name: String, number: Int)
  case class SUCCESS(name: String, number: Int)
  case class FAIL(name: String, number: Int)
  case class End()

  case class Commanded(name: String, number: Int)
  case class Succeeded(name: String, number: Int)

  @Test def test() {
    val r = new M1
    setMonitor(r, false)

    addObj(COMMAND(name = "STOP_DRIVING", number = 1)) // 1
    facts(
      'Commanded('name -> "STOP_DRIVING", 'number -> 1))

    addObj(COMMAND(name = "START_CAMERA", number = 2)) // 2
    facts(
      'Commanded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "STOP_DRIVING", 'number -> 1))

    addObj(COMMAND(name = "TURN_ANTENNA", number = 3)) // 3
    facts(
      'Commanded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "TURN_ANTENNA", 'number -> 3),
      'Commanded('name -> "STOP_DRIVING", 'number -> 1))

    addObj(FAIL(name = "STOP_DRIVING", number = 1)) // 4
    facts(
      'Commanded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "TURN_ANTENNA", 'number -> 3))

    addObj(SUCCESS(name = "START_CAMERA", number = 2)) // 5
    facts(
      'Succeeded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "TURN_ANTENNA", 'number -> 3))

    addObj(COMMAND(name = "STOP_DRIVING", number = 4)) // 6
    facts(
      'Succeeded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "STOP_DRIVING", 'number -> 4),
      'Commanded('name -> "TURN_ANTENNA", 'number -> 3))

    addObj(SUCCESS(name = "START_CAMERA", number = 2)) // 7
    facts(
      'Succeeded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "STOP_DRIVING", 'number -> 4),
      'Commanded('name -> "TURN_ANTENNA", 'number -> 3))

    addObj(End()) // 8
    facts(
      'Succeeded('name -> "START_CAMERA", 'number -> 2),
      'Commanded('name -> "STOP_DRIVING", 'number -> 4),
      'Commanded('name -> "TURN_ANTENNA", 'number -> 3))

    result(
      Report(
        "ERROR command not succeeded",
        (6, "r1.1", 'Commanded('name -> "STOP_DRIVING", 'number -> 4)),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR command not succeeded",
        (3, "r1.1", 'Commanded('name -> "TURN_ANTENNA", 'number -> 3)),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR two successes",
        (5, "r2.1", 'Succeeded('name -> "START_CAMERA", 'number -> 2)),
        (7, "r2.2", 'Fail("ERROR two successes"))),
      Report(
        "ERROR command failed before success",
        (1, "r1.1", 'Commanded('name -> "STOP_DRIVING", 'number -> 1)),
        (4, "r1.3", 'Fail("ERROR command failed before success"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M2: ---
// ===================

class M2 extends Monitor {
  case class COMMAND(one: Symbol, two: Symbol) extends Obs
  case class Commanded(one: Symbol) extends Obs

  // r3: Commands should be emitted with numbers increasing by one.

  "r3.1" -- COMMAND('*, 'x) |-> insert(Commanded('x))
  "r3.2" -- Commanded('x) == 'l & COMMAND('*, 'y) |-> rem('l)
  "r3.3" -- Commanded('x) & COMMAND('*, 'y) |-> {
    if ('y.i != 'x.i + 1) {
      fail("command numbers not increasing by one: " + 'x.i + " and " + 'y.i)
    }
  }
}

class Test1_2 extends Contract {
  @Test def test() {
    val r = new M2
    setMonitor(r, false)

    case class COMMAND(one: String, two: Int)

    addObj(COMMAND("STOP_DRIVING", 1)) // 1
    facts(
      'Commanded(1))

    addObj(COMMAND("START_CAMERA", 2)) // 2
    facts(
      'Commanded(2))

    addObj(COMMAND("STOP_CAMERA", 3)) // 3
    facts(
      'Commanded(3))

    addObj(COMMAND("START_RADIO", 4)) // 4
    facts(
      'Commanded(4))

    addObj(COMMAND("START_DRIVING", 6)) // 5
    facts(
      'Commanded(6))

    addObj(COMMAND("STOP_DRIVING", 7)) // 6
    facts(
      'Commanded(7))

    result(
      Report(
        "ERROR command numbers not increasing by one: 4 and 6",
        (4, "r3.1", 'Commanded(4)),
        (5, "r3.3", 'Fail("ERROR command numbers not increasing by one: 4 and 6"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M3: ---
// ===================

class M3 extends Monitor {
  case class COMMAND(one: +[String], two: +[Int]) extends Obs
  case class SUCCESS(one: +[String], two: +[Int]) extends Obs
  case class Commanded(one: +[String], two: +[Int]) extends Obs

  // r4: A command with a name N cannot be submitted with two different command
  //     numbers without a command success in between.

  "r4.1" -- COMMAND('n, 'x) |-> insert(Commanded('n, 'x))
  "r4.2" -- Commanded('n, 'x) == 'l & SUCCESS('n, 'x) |-> rem('l)
  "r4.3" -- Commanded('n, 'y) & COMMAND('n, 'x) |-> {
    if ('x.i != 'y.i)
      fail("command issued with different numbers before succeeding")
  }
}

class Test1_3 extends Contract {
  @Test def test() {
    val r = new M3
    setMonitor(r, false)

    case class COMMAND(one: String, two: Int)
    case class SUCCESS(one: String, two: Int)

    addObj(COMMAND("START_DRIVING", 1)) // 1
    facts(
      'Commanded("START_DRIVING", 1))

    addObj(COMMAND("STOP_DRIVING", 2)) // 2
    facts(
      'Commanded("STOP_DRIVING", 2),
      'Commanded("START_DRIVING", 1))

    addObj(SUCCESS("START_DRIVING", 1)) // 3
    facts(
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("STOP_DRIVING", 3)) // 4
    facts(
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("START_CAMERA", 2)) // 5
    facts(
      'Commanded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("STOP_CAMERA", 3)) // 6
    facts(
      'Commanded("STOP_CAMERA", 3),
      'Commanded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("STOP_CAMERA", 3)) // 7
    facts(
      'Commanded("STOP_CAMERA", 3),
      'Commanded("START_CAMERA", 2),
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("STOP_DRIVING", 7)) // 8
    facts(
      'Commanded("STOP_DRIVING", 2),
      'Commanded("STOP_CAMERA", 3),
      'Commanded("STOP_DRIVING", 7),
      'Commanded("STOP_DRIVING", 3),
      'Commanded("START_CAMERA", 2))

    result(
      Report(
        "ERROR command issued with different numbers before succeeding",
        (4, "r4.1", 'Commanded("STOP_DRIVING", 3)),
        (8, "r4.3", 'Fail("ERROR command issued with different numbers before succeeding"))),
      Report(
        "ERROR command issued with different numbers before succeeding",
        (2, "r4.1", 'Commanded("STOP_DRIVING", 2)),
        (8, "r4.3", 'Fail("ERROR command issued with different numbers before succeeding"))),
      Report(
        "ERROR command issued with different numbers before succeeding",
        (2, "r4.1", 'Commanded("STOP_DRIVING", 2)),
        (4, "r4.3", 'Fail("ERROR command issued with different numbers before succeeding"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M4: ---
// ===================

class M4 extends Monitor {
  case class COMMAND(one: +[String], two: +[Int]) extends Obs
  case class SUCCESS(one: +[String], two: +[Int]) extends Obs
  case class Commanded(one: +[String], two: +[Int]) extends Obs

  // r5: A command cannot succeed without a previous command having been issued.

  "r5.1" -- COMMAND('n, 'x) |-> insert(Commanded('n, 'x))
  "r5.2" -- Commanded('n, 'x) == 'l & SUCCESS('n, 'x) |-> rem('l)
  "r5.3" -- SUCCESS('n, 'x) & not(Commanded('n, 'x)) |-> error("Success of non issued command")

}

class Test1_4 extends Contract {
  @Test def test() {
    val r = new M4
    setMonitor(r, false)

    case class COMMAND(one: String, two: Int)
    case class SUCCESS(one: String, two: Int)

    addObj(COMMAND("START_DRIVING", 1)) // 1
    facts(
      'Commanded("START_DRIVING", 1))

    addObj(COMMAND("STOP_DRIVING", 2)) // 2
    facts(
      'Commanded("STOP_DRIVING", 2),
      'Commanded("START_DRIVING", 1))

    addObj(SUCCESS("START_DRIVING", 1)) // 3
    facts(
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("STOP_DRIVING", 3)) // 4
    facts(
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2))

    addObj(COMMAND("START_CAMERA", 4)) // 5
    facts(
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2),
      'Commanded("START_CAMERA", 4))

    addObj(SUCCESS("STOP_DRIVE", 1)) // 6
    facts(
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2),
      'Commanded("START_CAMERA", 4))

    addObj(COMMAND("STOP_DRIVING", 5)) // 7
    facts(
      'Commanded("STOP_DRIVING", 3),
      'Commanded("STOP_DRIVING", 2),
      'Commanded("START_CAMERA", 4),
      'Commanded("STOP_DRIVING", 5))

    result(
      Report( // this flags as a Scala type error, but it is not believed to be.
        "ERROR Success of non issued command",
        (6, "r5.3", 'Fail("ERROR Success of non issued command"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M5: ---
// ===================

class M5 extends Monitor {
  case class COMMAND(one: +[String], two: +[Int], three: +[Int]) extends Obs
  case class SUCCESS(one: +[String], two: +[Int], three: +[Int]) extends Obs
  case class End() extends Obs
  case class Commanded(one: +[Int], two: +[Int]) extends Obs

  // r6: When a DRIVE command is issued a success should occur within 10 seconds.

  val (n, x, l, t, t1, t2) = ('n, 'x, 'l, 't, 't1, 't2)

  "r6.1" -- COMMAND("DRIVE", x, t) |-> add(Commanded(x, t))
  "r6.2" -- SUCCESS("DRIVE", x, t2) & Commanded(x, t1) |-> {
    if (t2.i - t1.i > 10000) {
      fail("SUCCESS after deadline")
    }
  }
  "r6.3" -- Commanded(x, t1) == l & SUCCESS("DRIVE", x, t2) |-> rem(l)
  "r6.4" -- END() & Commanded(x, t) |-> error("DRIVE command never succeeded")

}

class Test1_5 extends Contract {
  @Test def test() {
    val r = new M5
    setMonitor(r, false)

    case class COMMAND(one: String, two: Int, three: Int)
    case class SUCCESS(one: String, two: Int, three: Int)
    case class END()

    addObj(COMMAND("DRIVE", 1, 1000)) // 1
    facts(
      'Commanded(1, 1000))

    addObj(COMMAND("TURN", 2, 2000)) // 2
    facts(
      'Commanded(1, 1000))

    addObj(COMMAND("STOP", 3, 3000)) // 3
    facts(
      'Commanded(1, 1000))

    addObj(SUCCESS("DRIVE", 1, 7000)) // 4
    facts()

    addObj(SUCCESS("TURN", 2, 22000)) // 5
    facts()

    addObj(COMMAND("DRIVE", 1, 30000)) // 6
    facts(
      'Commanded(1, 30000))

    addObj(SUCCESS("DRIVE", 1, 50000)) // 7
    facts()

    addObj(END()) // 8
    facts()

    result(
      Report(
        "ERROR SUCCESS after deadline",
        (6, "r6.1", 'Commanded(1, 30000)),
        (7, "r6.2", 'Fail("ERROR SUCCESS after deadline"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M6: ---
// ===================

// Formulated as maps.

class M6 extends Monitor {
  case class COMMAND(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class SUCCESS(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class FAIL(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class End() extends Obs

  case class Commanded(name: +[Int], number: +[Int]) extends Obs
  case class Succeeded(one: +[Int], two: +[Int]) extends Obs

  // r1: A command should succeed without failing first.

  "r1.1" -- COMMAND(name = 'n, number = 'x) |-> insert(Commanded(name = 'n, number = 'x))
  "r1.2" -- Commanded(name = 'n, number = 'x) == 'cmd & SUCCESS(name = 'n, number = 'x) |-> rem('cmd)
  "r1.3" -- Commanded(name = 'n, number = 'x) == 'cmd & FAIL(name = 'n, number = 'x) |->
    (rem('cmd), error("command failed before success"))
  "r1.4" -- END() & Commanded(name = 'n, number = 'x) |-> error("command not succeeded")

  // r2: command should succeed at most once

  "r2.1" -- SUCCESS(name = 'n, number = 'x) & not(Succeeded('n, 'x)) |-> insert(Succeeded('n, 'x))
  "r2.2" -- SUCCESS(name = 'n, number = 'x) & Succeeded('n, 'x) |-> error("two successes")
}

class Test1_6 extends Contract {
  case class COMMAND(name: String, number: Int, time: Int)
  case class SUCCESS(name: String, number: Int, time: Int)
  case class FAIL(name: String, number: Int, time: Int)
  case class END()

  @Test def test() {
    val r = new M6
    setMonitor(r, false)

    addObj(COMMAND(name = "STOP_DRIVING", number = 1, time = 1000)) // 1
    facts(
      'Commanded('number -> 1, 'name -> "STOP_DRIVING"))

    addObj(COMMAND(name = "START_CAMERA", number = 2, time = 2000)) // 2
    facts(
      'Commanded('number -> 2, 'name -> "START_CAMERA"),
      'Commanded('number -> 1, 'name -> "STOP_DRIVING"))

    addObj(COMMAND(name = "TURN_ANTENNA", number = 3, time = 3000)) // 3
    facts(
      'Commanded('number -> 2, 'name -> "START_CAMERA"),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 1, 'name -> "STOP_DRIVING"))

    addObj(FAIL(name = "STOP_DRIVING", number = 1, time = 4000)) // 4
    facts(
      'Commanded('number -> 2, 'name -> "START_CAMERA"),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"))

    addObj(SUCCESS(name = "START_CAMERA", number = 2, time = 5000)) // 5
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"))

    addObj(COMMAND(name = "STOP_DRIVING", number = 4, time = 6000)) // 6
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 4, 'name -> "STOP_DRIVING"))

    addObj(SUCCESS(name = "START_CAMERA", number = 2, time = 7000)) // 7
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 4, 'name -> "STOP_DRIVING"))

    addObj(END()) // 8
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 4, 'name -> "STOP_DRIVING"))

    result(
      Report(
        "ERROR command not succeeded",
        (6, "r1.1", 'Commanded('number -> 4, 'name -> "STOP_DRIVING")),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR command not succeeded",
        (3, "r1.1", 'Commanded('number -> 3, 'name -> "TURN_ANTENNA")),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR two successes",
        (5, "r2.1", 'Succeeded("START_CAMERA", 2)),
        (7, "r2.2", 'Fail("ERROR two successes"))),
      Report(
        "ERROR command failed before success",
        (1, "r1.1", 'Commanded('number -> 1, 'name -> "STOP_DRIVING")),
        (4, "r1.3", 'Fail("ERROR command failed before success"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M7: ---
// ===================

// Exercising different ways of generating events as monitor input.

//  [x]  addMapEvent(map: Map[Symbol, Any])
//  [x]  addMapEvent(fields: (Symbol, Any)*)
//  [x]  addMapEvent(kind: Symbol)(fields: (Symbol, Any)*)
//  [x]  addEvent(kind: Symbol)(values: Any*)

class M7 extends Monitor {
  case class COMMAND(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class SUCCESS(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class FAIL(name: +[String], number: +[Int], time: +[Int] = null) extends Obs
  case class End() extends Obs

  case class Commanded(name: +[Int], number: +[Int]) extends Obs
  case class Succeeded(one: +[Int], two: +[Int]) extends Obs

  // r1: A command should succeed without failing first.

  "r1.1" -- COMMAND(name = 'n, number = 'x) |-> insert(Commanded(name = 'n, number = 'x))
  "r1.2" -- Commanded(name = 'n, number = 'x) == 'cmd & SUCCESS(name = 'n, number = 'x) |-> rem('cmd)
  "r1.3" -- Commanded(name = 'n, number = 'x) == 'cmd & FAIL(name = 'n, number = 'x) |->
    (rem('cmd), error("command failed before success"))
  "r1.4" -- END() & Commanded(name = 'n, number = 'x) |-> error("command not succeeded")

  // r2: command should succeed at most once

  "r2.1" -- SUCCESS(name = 'n, number = 'x) & not(Succeeded('n, 'x)) |-> insert(Succeeded('n, 'x))
  "r2.2" -- SUCCESS(name = 'n, number = 'x) & Succeeded('n, 'x) |-> error("two successes")
}

class Test1_7 extends Contract {
  case class COMMAND(name: String, number: Int, time: Int)
  case class SUCCESS(name: String, number: Int, time: Int)
  case class FAIL(name: String, number: Int, time: Int)
  case class END()

  @Test def test() {
    val r = new M7
    setMonitor(r, false)

    addObj(COMMAND(name = "STOP_DRIVING", number = 1, time = 1000)) // 1
    facts(
      'Commanded('number -> 1, 'name -> "STOP_DRIVING"))

    addObj(COMMAND(name = "START_CAMERA", number = 2, time = 2000)) // 2
    facts(
      'Commanded('number -> 2, 'name -> "START_CAMERA"),
      'Commanded('number -> 1, 'name -> "STOP_DRIVING"))

    addObj(COMMAND(name = "TURN_ANTENNA", number = 3, time = 3000)) // 3
    facts(
      'Commanded('number -> 2, 'name -> "START_CAMERA"),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 1, 'name -> "STOP_DRIVING"))

    addObj(FAIL(name = "STOP_DRIVING", number = 1, time = 4000)) // 4
    facts(
      'Commanded('number -> 2, 'name -> "START_CAMERA"),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"))

    addObj(SUCCESS(name = "START_CAMERA", number = 2, time = 5000)) // 5
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"))

    addObj(COMMAND(name = "STOP_DRIVING", number = 4, time = 6000)) // 6
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 4, 'name -> "STOP_DRIVING"))

    addObj(SUCCESS(name = "START_CAMERA", number = 2, time = 7000)) // 7
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 4, 'name -> "STOP_DRIVING"))

    addObj(END()) // 8
    facts(
      'Succeeded("START_CAMERA", 2),
      'Commanded('number -> 3, 'name -> "TURN_ANTENNA"),
      'Commanded('number -> 4, 'name -> "STOP_DRIVING"))

    result(
      Report(
        "ERROR command not succeeded",
        (6, "r1.1", 'Commanded('number -> 4, 'name -> "STOP_DRIVING")),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR command not succeeded",
        (3, "r1.1", 'Commanded('number -> 3, 'name -> "TURN_ANTENNA")),
        (8, "r1.4", 'Fail("ERROR command not succeeded"))),
      Report(
        "ERROR two successes",
        (5, "r2.1", 'Succeeded("START_CAMERA", 2)),
        (7, "r2.2", 'Fail("ERROR two successes"))),
      Report(
        "ERROR command failed before success",
        (1, "r1.1", 'Commanded('number -> 1, 'name -> "STOP_DRIVING")),
        (4, "r1.3", 'Fail("ERROR command failed before success"))))

    //r.draw("test/output/scalashow.dot")
  }
}

// ===================
// --- Monitor M8: ---
// ===================

// Testing a long sequence of events to see how error traces should
// be constructed.

class M8 extends Monitor {

  // A command should go through the following steps:
  // - it is issued
  // - it is dispatched without a dispatch failure before
  // - it is executed without an execution failure before
  // - it succeeds without a failure before

  case class COMMAND(one: +[String], two: +[Int]) extends Obs
  case class DISPATCH_ERR(one: +[String], two: +[Int]) extends Obs
  case class DISPATCH(one: +[String], two: +[Int]) extends Obs
  case class EXEC_ERR(one: +[String], two: +[Int]) extends Obs
  case class EXECUTE(one: +[String], two: +[Int]) extends Obs
  case class SUCCESS(one: +[String], two: +[Int]) extends Obs
  case class FAIL(one: +[String], two: +[Int]) extends Obs
  case class VERIFY(one: +[String], two: +[Int]) extends Obs
  case class End() extends Obs

  case class Commanded(one: +[Int], two: +[Int]) extends Obs
  case class Dispatched(one: +[Int], two: +[Int]) extends Obs
  case class Executing(one: +[Int], two: +[Int]) extends Obs
  case class Verified(one: +[Int], two: +[Int]) extends Obs

  "command" -- COMMAND('name, 'number) |-> insert(Commanded('name, 'number))
  "dispatch error" -- Commanded('n, 'x) & DISPATCH_ERR('n, 'x) |-> report(s"dispatch error (${'n.s}},${'x.i}}) ")
  "dispatch" -- Commanded('n, 'x) & DISPATCH('n, 'x) |-> replace('Commanded)(Dispatched('n, 'x))
  "execution error" -- Dispatched('n, 'x) & EXEC_ERR('n, 'x) |-> fail(s"execution error (${'n.s}},${'x.i}})")
  "execute" -- Dispatched('n, 'x) & EXECUTE('n, 'x) |-> replace('Dispatched)(Executing('n, 'x))
  "failure" -- Executing('n, 'x) & Verified('n, 'x) & FAIL('n, 'x) |-> fail() //(s"FAIL when verified - (${'n.s},${'x.i})")
  "succeed" -- Executing('n, 'x) & SUCCESS('n, 'x) |-> remove('Executing)
  "verify" -- VERIFY('n, 'x) |-> insert(Verified('n, 'x))
  "any failure" -- FAIL('n, 'x) |-> fail(s"FAIL is just bad - (${'n.s},${'x.i})")
  "at the end" -- END() |-> {
    //println(getMonitorResult)
  }
}

class Test1_8 extends Contract {
  case class COMMAND(one: String, two: Int)
  case class DISPATCH_ERR(one: String, two: Int)
  case class DISPATCH(one: String, two: Int)
  case class EXEC_ERR(one: String, two: Int)
  case class EXECUTE(one: String, two: Int)
  case class SUCCESS(one: String, two: Int)
  case class FAIL(one: String, two: Int)
  case class VERIFY(one: String, two: Int)
  case class END()

  @Test def test() {
    val m = new M8
    setMonitor(m, false)

    addObj(COMMAND("START_DRIVING", 1)) // 1
    facts(
      'Commanded("START_DRIVING", 1))

    addObj(COMMAND("CALL_HOME", 2)) // 2
    facts(
      'Commanded("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(DISPATCH_ERR("START_DRIVING", 1)) // 3
    facts(
      'Commanded("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(DISPATCH("CALL_HOME", 2)) // 4
    facts(
      'Dispatched("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(EXECUTE("START_DRIVING", 1)) // 5
    facts(
      'Dispatched("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(EXECUTE("CALL_HOME", 2)) // 6
    facts(
      'Executing("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(VERIFY("CALL_HOME", 2)) // 7
    facts(
      'Verified("CALL_HOME", 2),
      'Executing("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(SUCCESS("START_DRIVING", 1)) // 8
    facts(
      'Verified("CALL_HOME", 2),
      'Executing("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(FAIL("CALL_HOME", 2)) // 9
    facts(
      'Verified("CALL_HOME", 2),
      'Executing("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    addObj(END()) // 10
    facts(
      'Verified("CALL_HOME", 2),
      'Executing("CALL_HOME", 2),
      'Commanded("START_DRIVING", 1))

    result(
      Report(
        "ERROR",
        (2, "command", 'Commanded("CALL_HOME", 2)),
        (4, "dispatch", 'Dispatched("CALL_HOME", 2)),
        (6, "execute", 'Executing("CALL_HOME", 2)),
        (7, "verify", 'Verified("CALL_HOME", 2)),
        (9, "failure", 'Fail("ERROR"))),
      Report( // this flags as a Scala type error, but it is not believed to be.
        "ERROR FAIL is just bad - (CALL_HOME,2)",
        (9, "any failure", 'Fail("ERROR FAIL is just bad - (CALL_HOME,2)"))),
      Report(
        "dispatch error (START_DRIVING},1}) ",
        (1, "command", 'Commanded("START_DRIVING", 1)),
        (3, "dispatch error", 'Fail("dispatch error (START_DRIVING},1}) "))))

    val failureRules = m.getMonitorResult.getHistories filter {
      case history => history.rule contains "failure"
    }
    assert(failureRules.size == 2, "\nEXPECTED TWO FAILURE RULES\n")
  }
}
