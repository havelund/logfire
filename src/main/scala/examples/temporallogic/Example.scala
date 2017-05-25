package examples.temporallogic

import rete._

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.language.postfixOps


// ----------------------
// Specification Patterns
// ----------------------

trait LogicUtil extends Monitor {
  var ruleId = 0

  def newRuleId(): String = {
    ruleId += 1
    "r" + ruleId
  }
}

trait TemporalLogic extends LogicUtil {
  private def response(name: String)(pc1: PC, pc2: PC) {
    val unsafe = newSymbol('unsafe)
    val args = pc1.getVariables.reverse
    newRuleId() -- pc1 |-> unsafe(args: _*)
    newRuleId() -- unsafe(args: _*) & pc2 |-> remove(unsafe)
    newRuleId() -- unsafe(args: _*) & 'end() |-> fail(name)
  }

  private def precedence(name: String)(pc1: PC, pc2: PC) {
    val safe = newSymbol('safe)
    val args = pc2.getVariables.reverse
    newRuleId() -- pc2 |-> safe(args: _*)
    newRuleId() -- pc1 & not(safe(args: _*)) |-> fail(name)
  }

  implicit def syntax(name: String) = new {
    def ---(pc1: PC) = new {
      def -->(pc2: PC) = response(name)(pc1, pc2)

      def ==>(pc2: PC) = precedence(name)(pc1, pc2)
    }
  }
}

// ----------------------
// Trying Temporal Logic.
// ----------------------

class CommandMonitor1 extends TemporalLogic {
  "commands must succeed" ---
    'COMMAND('x, 'y) --> 'SUCCESS('x, 'y)

  "commands cannot succeed without having been issued" ---
    'SUCCESS('x, 'y) ==> 'COMMAND('x, 'y)
}

object DemoCommandMonitor1 {
  def main(args: Array[String]) {
    val m = new CommandMonitor1

    m.printRules()
    m.PRINT = true

    m.addEvent('COMMAND)("STOP_DRIVING", 1)
    m.addEvent('COMMAND)("START_CAMERA", 2)

    m.addEvent('SUCCESS)("STOP_DRIVING", 1)
    m.addEvent('SUCCESS)("START_CAMERA", 3)

    m.addEvent('end)()
  }
}

class Test extends Monitor {
  val e = fact

  "r1" -- e('x,'y) |-> {
    e(3)
  }
  
  "r2" -- e(3) |-> {
    println(getArgs(e))
  }
}

object DemoCommandMonitor2 {
  def main(args: Array[String]) {
    val m = new Test

    m.printRules()
    m.PRINT = true

    m.addEvent('e)(1,2)
  }
}

