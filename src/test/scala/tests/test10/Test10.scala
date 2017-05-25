
package tests.test10

import rete._
import org.junit.Test

class OneCommandWithSymbols extends Monitor {
  val EVR = event
  val Commanded = fact

  "r1" -- EVR('name -> 'n, 'message -> "START") & not(Commanded('n)) |-> insert(Commanded('n))
  "r2" -- EVR('name -> 'n, 'message -> "START") & Commanded('n) |-> fail("two commands")
  "r3" -- EVR('name -> 'n, 'message -> "STOP") & Commanded('n) |-> remove('Commanded)
}

class RunWithSymbols extends Contract {
  @Test def test() {
    val m = new OneCommandWithSymbols
    setMonitor(m, false)

    add('EVR('name -> "reboot", 'esclk -> 1000, 'message -> "START", 'level -> 0))
    facts(
      'Commanded("reboot"))

    add('EVR('name -> "reboot", 'esclk -> 2000, 'message -> "STOP", 'level -> 0))
    facts()

    add('EVR('name -> "turn", 'esclk -> 3000, 'message -> "START", 'level -> 0))
    facts(
      'Commanded("turn"))

    add('EVR('name -> "turn", 'esclk -> 4000, 'message -> "START", 'level -> 0))
    facts(
      'Commanded("turn"))

    add('EVR('name -> "turn", 'esclk -> 5000, 'message -> "STOP", 'level -> 0))
    facts()

    result(
      Report(
        "ERROR two commands",
        (3, "r1", 'Commanded("turn")),
        (4, "r2", 'Fail("ERROR two commands"))))
  }
}

class OneCommandWithClasses extends Monitor {
  case class EVR(
    name: +[String] = null,
    esclk: +[Long] = null,
    message: +[String] = null,
    level: +[Int] = null) extends Obs

  case class Commanded(name: +[String]) extends Obs

  "r1" -- EVR(name = 'n, message = "START") & not(Commanded('n)) |-> insert(Commanded('n))
  "r2" -- EVR(name = 'n, message = "START") & Commanded('n) |-> fail("two commands")
  "r3" -- EVR(name = 'n, message = "STOP") & Commanded('n) |-> remove('Commanded)
}

class RunWithClasses extends Contract {
  case class EVR(name: String, esclk: Long, message: String, level: Int)

  @Test def test() {
    val m = new OneCommandWithClasses
    setMonitor(m, false)

    addObj(EVR("reboot", 1000, "START", 0))
    facts(
      'Commanded('name -> "reboot"))

    addObj(EVR("reboot", 2000, "STOP", 0))
    facts()

    addObj(EVR("turn", 3000, "START", 0))
    facts(
      'Commanded('name -> "turn"))

    addObj(EVR("turn", 4000, "START", 0))
    facts(
      'Commanded('name -> "turn"))

    addObj(EVR("turn", 5000, "STOP", 0))
    facts()

    result(
      Report(
        "ERROR two commands",
        (3, "r1", 'Commanded('name -> "turn")),
        (4, "r2", 'Fail("ERROR two commands"))))
  }
}
