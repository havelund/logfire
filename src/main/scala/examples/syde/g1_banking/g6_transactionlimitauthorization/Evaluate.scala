package examples.syde.g1_banking.g6_transactionlimitauthorization

import rete._

/**
 * Transaction limit autorization.
 *
 * This property is similar to the previous property but in this case the
 * property requires that executed transactions of any customer must be
 * authorised by some employee (2-20 days) before they are executed if the
 * transferred money exceeds a given threshold of $2,000.
 */

class TransactionLimitAuthorization extends Monitor {
  "r1" -- 'auth('t, 'ts) & not('Auth('t, '_)) |-> insert('Auth('t, 'ts))

  "r2" -- 'Auth('t, '_) & 'auth('t, 'ts) |-> update('Auth('t, 'ts))
  
  "r3" -- 'Auth('t, 'ts1) & 'trans('t, 'a, 'ts2) |-> {
    ensure('a < 2000 || ('ts2 - 'ts1 >= 2 && 'ts2 - 'ts1 < 21))
    remove('Auth)
  }

  "r4" -- 'trans('t, 'a, '_) & not('Auth('t, '_)) |-> {
    ensure('a < 2000)
  }
}

object Evaluate extends MonitorFeeder {
  val trans = 'trans
  val auth = 'auth

  val trace1 = List(trans("A", 1999, 1), trans("B", 1900, 2))
  val trace2 = List(auth("A", 1), trans("A", 1999, 2), trans("B", 1900, 3))
  val trace3 = List(auth("A", 1), trans("A", 2000, 4), trans("B", 1900, 5))
  val trace4 = List(
    trans("A", 1000, 1),
    trans("B", 1100, 2),
    auth("D", 3),
    trans("C", 1200, 4),
    auth("E", 5),
    trans("D", 2100, 6),
    trans("E", 2200, 7),
    auth("G", 8),
    trans("F", 1300, 9),
    trans("G", 1900, 100))

  val trace_1 = List(
    trans("A", 1000, 1),
    trans("B", 1100, 2),
    auth("D", 3),
    trans("C", 1200, 4),
    auth("E", 5),
    trans("D", 2100, 100),
    trans("E", 2200, 107),
    auth("G", 108),
    trans("F", 1300, 109),
    trans("G", 1900, 110))
    
  val trace_2 = List(auth("A", 1), trans("A", 2000, 2), trans("B", 2000, 3))

  def main(args: Array[String]) {
    val m = new TransactionLimitAuthorization
    m.PRINT = true
    trace_2 foreach m.addMapEvent
  }
}
