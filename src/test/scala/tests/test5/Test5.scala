package tests.test5

// there is no delay yet

import rete._
import scala.language.reflectiveCalls
import org.junit.Test

trait CommandMonitor extends Monitor {
  val COMMAND, SUCCESS, FAIL = event
}

// Testing new notation for writing rules

class M1 extends CommandMonitor {
  val Commanded, Succeeded = fact

  // ---------------------------------------------------------
  // r1: A START command should succeed without failing first.
  // ---------------------------------------------------------

  "r1.1" -- COMMAND('name, 'number) |-> {
    if ('name.s.startsWith("START"))
      insert(Commanded('name, 'number))
  }

  "r1.2" -- Commanded('n, 'x) == 'cmd & SUCCESS('n, 'x) |-> rem('cmd)

  "r1.3" -- Commanded('n, 'x) == 'cmd & FAIL('n, 'x) |-> (rem('cmd), error("command failed before success"))

  "r1.4" -- END() & Commanded('n, 'x) |-> error("command not succeeded")

  // ---------------------------------------
  // r2: command should succeed at most once
  // ---------------------------------------

  "r2.1" -- SUCCESS('n, 'x) & not(Succeeded('n, 'x)) |-> Succeeded('n, 'x)

  "r2.2" -- SUCCESS('n, 'x) & Succeeded('n, 'x) |-> error("two successes")

}

class Test5_1 extends Contract {
  @Test def test() {
    val m = new M1
    setMonitor(m, false)

    m.addEvent('COMMAND)("STOP_DRIVING", 1) // 1
    facts()

    m.addEvent('COMMAND)("START_CAMERA", 2) // 2
    facts(
      'Commanded("START_CAMERA", 2))

    m.addEvent('COMMAND)("TURN_ANTENNA", 3) // 3
    facts(
      'Commanded("START_CAMERA", 2))

    m.addEvent('FAIL)("STOP_DRIVING", 1) // 4
    facts(
      'Commanded("START_CAMERA", 2))

    m.addEvent('SUCCESS)("START_CAMERA", 2) // 5
    facts(
      'Succeeded("START_CAMERA", 2))

    m.addEvent('COMMAND)("STOP_DRIVING", 4) // 6
    facts(
      'Succeeded("START_CAMERA", 2))

    m.addEvent('SUCCESS)("START_CAMERA", 2) // 7
    facts(
      'Succeeded("START_CAMERA", 2))

    m.addEvent('END)() // 8
    facts(
      'Succeeded("START_CAMERA", 2))

    result(
      Report(
        "ERROR two successes",
        (5, "r2.1", 'Succeeded("START_CAMERA", 2)),
        (7, "r2.2", 'Fail("ERROR two successes"))))
  }
}

