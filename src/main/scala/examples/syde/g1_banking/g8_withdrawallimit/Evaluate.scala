package examples.syde.g1_banking.g8_withdrawallimit

import rete._

/**
 * Withdrawal limit.
 *
 * The property is rooted in the domain of fraud detection. The property
 * requires that the sum of withdrawals of each user in the last 28
 * days does not exceed the limit of $10,000.
 */

class WithdrawalLimit extends Monitor {
  "r1" -- 'withdraw('u, 'a, 'ts) |-> {
    if ('a <= 10000)
      insert('Withdrawn('u, 'a, 'ts))
    else
      fail()
  }

  "r2" -- 'Withdrawn('u, 'sum, 'ts1) & 'withdraw('u, 'a, 'ts2) |-> {
    if ('ts2 - 'ts1 <= 28) {
      val newSum = 'sum + 'a
      if (newSum <= 10000)
        update('Withdrawn('u, newSum, 'ts1))
      else
        fail()
    } else remove('Withdrawn)
  }
}

object Evaluate extends MonitorFeeder {
  val withdraw = 'withdraw

  val trace1 = List(
    withdraw("John", 1000, 1),
    withdraw("Peter", 2000, 2),
    withdraw("John", 7000, 3),
    withdraw("John", 1000, 4),
    withdraw("John", 5000, 50),
    withdraw("Peter", 9000, 60))

  val trace_1 = List(
    withdraw("John", 1000, 1),
    withdraw("Peter", 2000, 2),
    withdraw("John", 7000, 3),
    withdraw("John", 1000, 4),
    withdraw("John", 5000, 5),
    withdraw("Peter", 9000, 6))

  def main(args: Array[String]) {
    val m = new WithdrawalLimit
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
