package examples.rv2016tutorial.maxwithdrawal

import rete._

class MaxWithdrawal extends Monitor {
  val withdraw = event
  val Withdrawn = fact

  "r1" -- withdraw('u, 'a, 't) |-> {
    if ('a <= 10000)
      insert(Withdrawn('u, 'a, 't))
    else
      fail()
  }

  "r2" -- Withdrawn('u, 'sum, 't1) & withdraw('u, 'a, 't2) |-> {
    if ('t2 - 't1 <= 30) {
      val newSum = 'sum + 'a
      if (newSum <= 10000)
        update('Withdrawn('u, newSum, 't1))
      else
        fail()
    } else remove('Withdrawn)
  }
}

object ApplyMonitor {
  val withdraw = 'withdraw

  def main(args: Array[String]) {
    val m = new MaxWithdrawal
    m.PRINT = true

    m.addEvent(withdraw)("John", 1000, 1)
    m.addEvent(withdraw)("Peter", 2000, 20)
    m.addEvent(withdraw)("John", 7000, 23)
    m.addEvent(withdraw)("John", 1000, 27)
    m.addEvent(withdraw)("John", 1001, 30)
    m.addEvent(withdraw)("Peter", 9000, 51)
  }
}
