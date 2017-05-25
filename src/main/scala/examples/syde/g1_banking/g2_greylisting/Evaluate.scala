package examples.syde.g1_banking.g2_greylisting

import rete._

/**
 * Grey listing.
 *
 * Once greylisted, a user must perform at least three incoming
 * transfers before being whitelisted. Therefore a user may only be whiltelisted
 * if there have been at least three transfers since the previous greylisting.
 *
 */

class GreyListing extends Monitor {
  "r1" -- 'greyList('u) & not('Grey('u, '_)) |-> insert('Grey('u, 0))
  "r2" -- 'Grey('u, '_) & 'greyList('u) |-> update('Grey('u, 0))
  "r3" -- 'Grey('u, 'count) & 'transfer('u) |-> update('Grey('u, 'count + 1))
  "r4" -- 'Grey('u, 'count) & 'whiteList('u) |-> {
    if ('count < 3)
      fail()
    else
      remove('Grey)
  }
}

object Evaluate extends MonitorFeeder {
  val (greyList, whiteList, transfer) = ('greyList, 'whiteList, 'transfer)
  val (a, b, c) = ("A", "B", "C")

  val trace1 = List(greyList(a), transfer(a), transfer(a), transfer(a), whiteList(a))
  val trace2 = List(whiteList(a), transfer(a), transfer(b), whiteList(b))
  val trace3 = List(transfer(a), greyList(b), transfer(c))
  val trace_1 = List(greyList(a), transfer(a), transfer(a), whiteList(a))
  val trace_2 = List(greyList(a), whiteList(a))

  def main(args: Array[String]) {
    val m = new GreyListing
    m.PRINT = true
    trace_2 foreach m.addMapEvent
  }
}
