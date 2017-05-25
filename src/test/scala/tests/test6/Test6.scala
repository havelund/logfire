
package tests.test6

import rete._
import scala.language.reflectiveCalls
import org.junit.Test

/**
 * This test was the first one created to use the `Test` class. It tests general
 * straight forward concepts.
 */

class M1 extends Monitor {
  val COMMAND, SUCCESS, FAIL = event
  val Commanded, Succeeded = fact

  // ---------------------------------------------------------
  // r1: A START command should succeed without failing first.
  // ---------------------------------------------------------

  "r1.1" -- COMMAND('name, 'number) |-> insert(Commanded('name, 'number))

  "r1.2" -- Commanded('n, 'x) == 'cmd & SUCCESS('n, 'x) |-> remove('cmd)

  "r1.3" -- Commanded('n, 'x) == 'cmd & FAIL('n, 'x) |-> {
    remove('cmd)
    fail("command failed before success")
  }

  "r1.4" -- END() & Commanded('n, 'x) |-> fail("command not succeeded")

  // ---------------------------------------
  // r2: command should succeed at most once
  // ---------------------------------------

  "r2.1" -- SUCCESS('n, 'x) & not(Succeeded('n, 'x)) |-> Succeeded('n, 'x)

  "r2.2" -- SUCCESS('n, 'x) & Succeeded('n, 'x) |-> fail("two successes")
}

class Test6_1 extends Contract {
  @Test def test() {
    val m = new M1
    setMonitor(m, false)

    add('COMMAND("STOP_DRIVING", 1)) // 1
    delta(
      'Commanded("STOP_DRIVING", 1))()

    add('COMMAND("START_CAMERA", 2)) // 2
    delta(
      'Commanded("START_CAMERA", 2))()

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
  }
}
