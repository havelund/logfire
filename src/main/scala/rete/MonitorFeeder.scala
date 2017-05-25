package rete

import scala.language.implicitConversions
import scala.language.reflectiveCalls

import Util._

/**
 * This class is meant to be extended by a class that instantiates and feeds a monitor
 * with events. The class makes it possible to write events and facts in the same way
 * they are written in specifications.
 */

class MonitorFeeder {

  /**
   * The type of facts as represented in tests: mappings from fields to values.
   */

  type Fact = Map[Symbol, Any]

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
}
