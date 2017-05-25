package examples.syde.g1_banking.g5_transactionlimitreporting

import rete._

/**
 * Transaction limit reporting.
 *
 * The property requires that executed transactions of any customer
 * must be reported within at most 5 days if the transferred money exceeds
 * a given threshold of $2,000.
 */

class TransactionLimitReporting extends Monitor {
  "r1" -- 'transfer('t, 'a, 'ts) |-> {
    if ('a > 2000) insert('Unsafe('t, 'ts))
  }

  "r2" -- 'Unsafe('t, 'ts1) & 'report('t, 'ts2) |-> {
    ensure('ts2 - 'ts1 <= 5)
    remove('Unsafe)
  }

  hot('Unsafe)
}

object Evaluate extends MonitorFeeder {
  val transfer = 'transfer
  val report = 'report

  val trace1 = List(transfer("A", 1999, 1), transfer("B", 2100, 2), report("B", 3))
  val trace_1 = List(
    transfer("A", 1999, 1),
    transfer("B", 2100, 2),
    transfer("C", 2200, 3),
    transfer("D", 2300, 4),
    transfer("E", 2400, 5),
    report("B", 6),
    report("C", 10))

  def main(args: Array[String]) {
    val m = new TransactionLimitReporting
    m.PRINT = true
    trace_1 foreach m.addMapEvent
    m.terminate() // needed in the precense of hot states
  }
}
