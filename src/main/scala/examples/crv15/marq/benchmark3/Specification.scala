package examples.crv15.marq.benchmark3

import rete._

/*
 * When an item is being sold an auction is created and recorded using the 
 * create_auction(item,minimum,period) event where minimum is the minimum 
 * price the item can be sold for and period is the number of days the auction 
 * will last. Each bid on the item must be strictly larger than the previous bid; 
 * bids are recorded by the event bid(item,amount). The auction is over when the 
 * period is over or the item is sold. An item can only be and must be sold if 
 * the last bid amount is greater than the minimum price; the sold(item) event 
 * records when an item is sold. After the auction is over no events related to 
 * the item are allowed. An item can only be placed for auction once. The passing 
 * of days is recorded by a propositional endOfDay event; the period of an auction 
 * is over when there have been period number of endOfDay events.
 * 
 * Events:
 * 
 * - create_auction(item,minimum,period)
 * - bid(item,amount)
 * - sold(item)
 * - endOfDay
 */

class CSVMonitor extends Monitor {
  import org.apache.commons.csv._

  override def submit(record: CSVRecord) {
    var map: Map[Symbol, Any] = Map();
    val kind = Symbol(record.get(0))
    map += 'kind -> kind
    kind match {
      // create_auction, item, minimum, period
      case 'create_auction =>
        map += 'one -> record.get(1)
        map += 'two -> record.get(2).toInt
        map += 'three -> record.get(3).toInt
      // bid, item, amount,
      case 'bid =>
        map += 'one -> record.get(1)
        map += 'two -> record.get(2).toInt
      // sold, item,,
      case 'sold =>
        map += 'one -> record.get(1)
      // endOfDay,,,
      case 'endOfDay =>
    }
    addMapEvent(map)
  }
}

class M_Slow extends CSVMonitor {
  val create_auction, bid, sold, endOfDay = event
  val Created, AuctionOver = fact

  "r1" -- create_auction('item, 'minimum, 'period) |->
    insert(Created('item, 'minimum, 'period, 0))

  "r2" -- Created('item, 'minimum, 'period, 'amount) & endOfDay() |-> {
    if ('period > 1)
      update(Created('item, 'minimum, 'period - 1, 'amount))
    else if ('amount < 'minimum)
      replace(Created)(AuctionOver('item))
    else
      fail("item not sold although amount >= minimum")
  }

  "r3" -- Created('item, 'minimum, 'period, 'oldamount) & bid('item, 'newamount) |-> {
    if ('newamount > 'oldamount)
      update(Created('item, 'minimum, 'period, 'newamount))
    else
      fail("bid not bigger than last bid")
  }

  "r4" -- Created('item, 'minimum, 'period, 'amount) & sold('item) |-> {
    if ('amount >= 'minimum) {
      replace(Created)(AuctionOver('item))
    } else
      fail("item sold below minimum value")
  }

  "r5" -- AuctionOver('item) & create_auction('item, '_, '_) |->
    fail("auction on item restarts after close")

  "r6" -- Created('item, '_, '_, '_) & create_auction('item, '_, '_) |->
    fail("auction on item restarts while auction is open")

  "r7" -- bid('item, '_) & not(Created('item, '_, '_, '_)) |->
    fail("bid on item that is not in auction")

  "r8" -- sold('item) & not(Created('item, '_, '_, '_)) |->
    fail("item sold that is not in auction")

  "r9" -- END() & Created('_, 'minimum, '_, 'amount) |-> {
    if ('amount >= 'minimum)
      fail("item not sold although amount >= minimum")
  }
}

class M_StillSlow extends CSVMonitor {
  val create_auction, bid, sold, endOfDay = event
  val Created = fact

  var auctionOver: Set[String] = Set()

  "r1" -- create_auction('item, 'minimum, 'period) |-> {
    if (auctionOver.contains('item.s))
      fail("auction on item restarts after close")
    else
      insert(Created('item, 'minimum, 'period, 0))
  }

  "r2" -- Created('item, 'minimum, 'period, 'amount) & endOfDay() |-> {
    if ('period > 1)
      update(Created('item, 'minimum, 'period - 1, 'amount))
    else if ('amount < 'minimum) {
      remove(Created)
      auctionOver += 'item.s
    } else
      fail("item not sold although amount >= minimum")
  }

  "r3" -- Created('item, 'minimum, 'period, 'oldamount) & bid('item, 'newamount) |-> {
    if ('newamount > 'oldamount)
      update(Created('item, 'minimum, 'period, 'newamount))
    else
      fail("bid not bigger than last bid")
  }

  "r4" -- Created('item, 'minimum, 'period, 'amount) & sold('item) |-> {
    if ('amount >= 'minimum) {
      remove(Created)
      auctionOver += 'item.s
    } else
      fail("item sold below minimum value")
  }

  "r5" -- Created('item, '_, '_, '_) & create_auction('item, '_, '_) |->
    fail("auction on item restarts while auction is open")

  "r6" -- bid('item, '_) & not(Created('item, '_, '_, '_)) |->
    fail("bid on item that is not in auction")

  "r7" -- sold('item) & not(Created('item, '_, '_, '_)) |->
    fail("item sold that is not in auction")

  "r8" -- END() & Created('_, 'minimum, '_, 'amount) |-> {
    if ('amount >= 'minimum)
      fail("item not sold although amount >= minimum")
  }
}

case class ItemRecord(minimum: Int, var period: Int) {
  var amount: Int = 0
}

class M extends CSVMonitor {
  val create_auction, bid, sold, endOfDay = event

  var itemsActive: Map[String, ItemRecord] = Map()
  var itemsDone: Set[String] = Set()

  "r1" -- create_auction('item, 'minimum, 'period) |-> {
    val item = 'item.s
    if (itemsActive.contains(item) || itemsDone.contains(item))
      fail("auction on item cannot begin again")
    else
      itemsActive += (item -> ItemRecord('minimum.i, 'period.i))
  }

  "r2" -- endOfDay() |-> {
    for ((item, record) <- itemsActive) {
      if (record.period > 1) {
        record.period -= 1
      } else if (record.amount < record.minimum) {
        itemsActive -= item
        itemsDone += item
      } else
        fail("item not sold although amount >= minimum")
    }
  }

  "r3" -- bid('item, 'newamount) |-> {
    val item = 'item.s
    if (itemsActive.contains(item)) {
      val record = itemsActive(item)
      val newAmount = 'newamount.i
      if (newAmount > record.amount)
        record.amount = newAmount
      else
        fail("bid not bigger than last bid")
    } else
      fail("bid on item that is not in auction")
  }

  "r4" -- sold('item) |-> {
    val item = 'item.s
    if (itemsActive.contains(item)) {
      val record = itemsActive(item)
      if (record.amount >= record.minimum) {
        itemsActive -= item
        itemsDone += item
      } else
        fail("item sold below minimum value")
    } else
      fail("item sold that is not in auction")
  }

  "r5" -- END() |-> {
    for ((item, record) <- itemsActive) {
      if (record.amount >= record.minimum)
        fail("item not sold although amount >= minimum")
    }
  }
}

object Evaluate extends MonitorFeeder {
  val create_auction = 'create_auction
  val bid = 'bid
  val sold = 'sold
  val endOfDay = 'endOfDay

  val trace1 = List(
    create_auction("hat", 4, 10),
    bid("hat", 1),
    bid("hat", 3),
    bid("hat", 4),
    sold("hat"))

  val trace2 = List(
    create_auction("hat", 4, 10),
    endOfDay(),
    endOfDay(),
    bid("hat", 3))

  val trace3 = List(
    create_auction("shoes", 50, 2),
    create_auction("hat", 4, 10),
    bid("shoes", 50),
    bid("hat", 3),
    sold("shoes"))

  val trace_1 = List(
    create_auction("hat", 5, 10),
    bid("hat", 1),
    bid("hat", 3),
    bid("hat", 4),
    sold("hat"))

  val trace_2 = List(
    create_auction("hat", 4, 1),
    endOfDay(),
    endOfDay(),
    bid("hat", 3))

  val trace_3 = List(
    create_auction("shoes", 50, 2),
    create_auction("hat", 4, 10),
    bid("shoes", 50),
    bid("hat", 5),
    sold("shoes"))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = true
    trace_3 foreach m.addMapEvent
    m.terminate()
  }
}
