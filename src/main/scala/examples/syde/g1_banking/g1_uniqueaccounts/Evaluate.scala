package examples.syde.g1_banking.g1_uniqueaccounts

import rete._

/**
 * Unique accounts.
 *
 * An account approved by the administrator may not have the same account
 * number as any other already existing account in the system.
 */

class UniqueAccounts extends Monitor {
  "r1" -- 'approve('id) |-> insert('Approved('id))
  "r2" -- 'Approved('id) & 'approve('id) |-> fail()
}

object Evaluate extends MonitorFeeder {
  val approve = 'approve
  val trace1 = List(approve(1), approve(2))
  val trace2 = List(approve(5), approve(3))
  val trace_1 = List(approve(1), approve(1))
  val trace_2 = List(approve(2), approve(1), approve(2))

  def main(args: Array[String]) {
    val m = new UniqueAccounts
    m.PRINT = true
    trace_2 foreach m.addMapEvent
   }
}
