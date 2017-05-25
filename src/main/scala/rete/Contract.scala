
package rete

import Util._
import scala.language.implicitConversions
import scala.language.reflectiveCalls

/**
 * This class offers features for testing LogFire - or for testing monitors. A test
 * class should extend this class to get access to the test methods.
 */

class Contract {
  private var previousFacts: Set[Fact] = Set()

  /**
   * The type of facts as represented in tests: mappings from fields to values.
   */

  type Fact = Map[Symbol, Any]

  /**
   * The specification of a single (error) report. It specifies what message
   * was reported, and then a line for each important event in the report.
   *
   * @param message the main message of the report.
   * @param lines the individual lines of the report, each indicating event number,
   *              what rule fired, and what fact was generated.
   */

  case class Report(message: String, lines: (Int, String, Fact)*) {
    override def toString: String = {
      var result = message + "\n"
      for ((eventNr, rule, fact) <- lines)
        result += s"$eventNr - $rule - ${PP.bindingToString(fact)}\n"
      result
    }
  }

  private var monitor: Monitor = null

  /**
   * Defines the monitor to be tested, such that the test class does not need to
   * repeatedly mention the monitor name.
   *
   * @param monitor the monitor to test.
   * @param print if true, verbose information is printed while the test runs, including
   *              the fact memories along the way, final result, and (error) reports as they are
   *              generated. The default value is false.
   */

  def setMonitor(monitor: Monitor, print: Boolean = false) {
    Contract.this.monitor = monitor
    monitor.PRINT = print
    monitor.PRINT_PROGRESS = print
  }

  /**
   * Lifts a symbol (an event/fact kind) to an object defining an `apply(arguments: Any*): Fact` function,
   * allowing events and facts to be written the same way they are written in specifications.
   *
   * @param kind the symbol (event or fact kind) to be lifted.
   * @return object allowing to define arguments to the event/fact in a natural and succinct manner.
   */

  implicit def kindToArguments(kind: Symbol) = new {
    def apply(arguments: Any*): Fact = {
      val nameOfPosition: Map[Int, Symbol] =
        positionalArguments
      var position: Int = 1
      var result: Map[Symbol, Any] = Map()
      for (arg <- arguments) {
        result +=
          (arg match {
            case (field: Symbol, value) => (field -> value)
            case _ => (nameOfPosition(position) -> arg)
          })
        position += 1
      }
      result + ('kind -> kind)
    }
  }

  /**
   * Verifies that the set of facts provided as arguments are exactly
   * those stored in the fact memory.
   *
   * @param facts the facts that are tested against the fact memory.
   */

  def facts(facts: Fact*) {
    val specFacts = facts.toSet
    val actualFacts = monitor.getMaps()
    val errorMessage =
      "\n-- FACTS EXPECTED: --\n" +
        specFacts.map(PP.bindingToString(_)).mkString("\n") +
        "\n-- ACTUAL FACTS: --\n" +
        actualFacts.map(PP.bindingToString(_)).mkString("\n") +
        "\n---\n"
    assert(specFacts == actualFacts, errorMessage)
    previousFacts = monitor.getMaps()
  }

  /**
   * Verifies that a given set of facts have been added, respectively removed, from
   * fact memory '''since start of this test or last call of `delta` or `facts`'''.
   * It can be convenient to use instead of calling `facts`,
   *
   * @param added the facts that are expected to have been added.
   * @param removed the facts that are expected to have been removed.
   */

  def delta(added: Fact*)(removed: Fact*) {
    val newFacts = monitor.getMaps()
    val addedFacts = newFacts -- previousFacts
    val removedFacts = previousFacts -- newFacts
    val addedError =
      "\n-- EXPECTED TO BE ADDED: --\n" +
        added.mkString("\n") +
        "\n-- ACTUALLY ADDED: --\n" +
        addedFacts.map(PP.bindingToString(_)).mkString("\n") +
        "\n---\n"
    val removedError =
      "\n-- EXPECTED TO BE REMOVED: --\n" +
        removed.mkString("\n") +
        "\n-- ACTUALLY REMOVED: --\n" +
        removedFacts.map(PP.bindingToString(_)).mkString("\n") +
        "\n---\n"
    assert(added.toSet == addedFacts, addedError)
    assert(removed.toSet == removedFacts, removedError)
    previousFacts = monitor.getMaps()
  }

  /**
   * Submits an event using positional style to the monitor set by `setMonitor`.
   *
   * @param kind the kind of the event.
   * @param values the arguments of the event.
   */

  def add(kind: Symbol)(values: Any*) {
    monitor.addEvent(kind)(values: _*)
  }

  /**
   * Submits an event to the monitor set by `setMonitor`.
   *
   * @param event the event to be added.
   */

  def add(event: Fact) {
    monitor.addMapEvent(event)
  }

  /**
   * Submits an event represented as an object to the monitor set by `setMonitor`.
   *
   * @param event the event object to be added.
   */

  def addObj(event: Any) {
    monitor.addObjEvent(event)
  }

  /**
   * Called to verify that `m.monitorResult` (where `setMonitor(m)` has been called earlier) satisfies
   * the sequence of report specifications provided as arguments.
   *
   * @param reports the list of reports that are expected. This is the specification that the implementation
   *                must satisfy.
   */

  def result(reports: Report*) {
    val specs = reports.toList
    val result = monitor.getMonitorResult
    val histories = result.getHistories
    var errorString: String =
      "\nTHE FOLLOWING RESULT:\n\n" +
        resultToSpec +
        "\n\nDOES NOT SATISFY THIS SPECIFICATION:\n\n" +
        specs.mkString("\n---\n") +
        "\n--- END ---\n"
    assert(histories.length == specs.length,
      notOkMessage(errorString)(s"history length: ${histories.length} != specs length: ${specs.length}"))
    for ((history, spec) <- histories.zip(specs)) verify(history, spec, errorString)
  }

  private def verify(history: History, spec: Report, errorString: String) {
    val msg = notOkMessage(errorString) _
    assert(history.message contains spec.message,
      s"\nMESSAGE: ${history.message}\nDOES NOT CONTAIN:\n${spec.message}\n")
    val historyLines = history.getLines
    assert(historyLines.length == spec.lines.length)
    for ((hline, (eventNr, ruleText, fact)) <- historyLines.zip(spec.lines)) {
      assert(hline.eventNumber == eventNr,
        msg(s"event nr: ${hline.eventNumber} != event nr: $eventNr"))
      assert(hline.rule contains ruleText,
        msg(s"rule: ${hline.rule} does not contain rule text: $ruleText"))
      assert(hline.fact == fact,
        msg(s"fact: ${PP.bindingToString(hline.fact)} != fact: ${PP.bindingToString(fact)}"))
    }
  }

  private def notOkMessage(body: String)(message: String): String =
    s"""\n\n\n===\n$message\n===\n$body"""

  //  result(
  //    Report(
  //      "ERROR command not succeeded",
  //      (3, "r1.1", 'Commanded("TURN_ANTENNA", 3)),
  //      (8, "r1.4", 'Fail("ERROR command not succeeded"))
  //    ), ...
  //  )

  private def extractRuleName(rule: String) = {
    val index1 = rule.indexOf("\"")
    val index2 = rule.indexOf("\"", index1 + 1)
    rule.slice(index1 + 1, index2)
  }

  private def resultToSpec: String = {
    val monitorResult = monitor.getMonitorResult
    val histories = monitorResult.getHistories
    val historiesAsStrings =
      for (history <- histories) yield {
        var result = "    Report(\n"
        val message = history.message
        result += s"""      "$message",\n"""
        val lines = history.getLines
        val linesAsStrings =
          for (Line(eventNumber, event, rule, fact) <- lines) yield {
            val ruleName = extractRuleName(rule)
            s"""      ($eventNumber, "$ruleName", ${PP.bindingToString(fact)})"""
          }
        result += linesAsStrings.mkString(",\n") + ")"
        result
      }
    "  result(\n" +
      historiesAsStrings.mkString(",\n") +
      "\n  )"
  }
}
