package examples.lncs10000

import rete._

import scala.language.implicitConversions
import scala.language.reflectiveCalls

/*
class P extends Monitor[Event] {
  require {
    case Command(_, "FSW", x, y) if x.startsWith("PWR") =>
      hot {
        case DispatchFailure(_, `x`, `y`) => error
        case Dispatch(t1, `x`, `y`) => hot {
          case Failure(_, `x`, `y`) => error
          case Success(t2, `x`, `y`) if t2 - t1 <= 5 =>
            state { case Success(_, `x`, `y`) => error }
        }
      } upto { case Command(_,"FSW",_,_) => true }
  }
}
*/


class CommandMonitor extends Monitor {
  val Command, Dispatch, Success, DispatchFailure, Failure = event
  val C, D, S = fact

  "n1" -- Command('_, "FSW", 'x, 'y) |-> {
    if ('x.s.startsWith("PWR")) insert(C('x, 'y))
  }
  "n2" -- C('x, 'y) & Dispatch('t1, 'x, 'y) |-> replace(C)(D('t1, 'x, 'y))
  "n3" -- D('t1, 'x, 'y) & Success('t2, 'x, 'y) |-> {
    if ('t2 - 't1 <= 5) replace(D)(S('x, 'y))
  }

  "f1" -- C('x, 'y) & DispatchFailure('_, 'x, 'y) |-> error
  "f2" -- D('_, 'x, 'y) & Failure('_, 'x, 'y) |-> error
  "f3" -- S('x, 'y) & Success('_, 'x, 'y) |-> error

  "h1" -- C('x, 'y) & Command('_, "FSW", '_, '_) |-> error
  "h2" -- D('_, 'x, 'y) & Command('_, "FSW", '_, '_) |-> error

  hot(C, D)
}


object Demo {
  def main(args: Array[String]) {
    val m = new CommandMonitor
    m.PRINT = true
    m.addEvent('Command)(10, "FSW", "PWR_OFF", 1)
    // m.addEvent('DispatchFailure)(11, "PWR_OFF", 1)
    m.addEvent('Dispatch)(20, "PWR_OFF", 1)
    // m.addEvent('Failure)(21, "PWR_OFF", 1)
    // m.addEvent('Command)(21, "FSW", "PWR_ON", 2)
    m.addEvent('Success)(25, "PWR_OFF", 1)
    // m.addEvent('Success)(30, "PWR_OFF", 1)
    m.terminate()
  }
}

