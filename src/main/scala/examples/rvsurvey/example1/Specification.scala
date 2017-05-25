package examples.rvsurvey.example1

import rete._

//import scala.language.implicitConversions
//import scala.language.reflectiveCalls

class MaxItemAuctionBidding extends Monitor {
  val list, bid, sold = event
  val Listed, Active, Sold = fact
  
  val N : Int = 2
  
  "r1" -- list('i,'min) & Listed('listed) |-> {
    if ('listed < N) {
      insert(Active('i,'min,0))
      update(Listed('listed + 1))
    } else fail()
  }
  
  "r2" -- bid('i,'a) & Active('i,'min,'last) |-> {
    if ('a > 'last) {
      update(Active('i,'min,'a))
    } else fail()
  }
  
  "r3" -- sold('i) & Active('i,'min,'last) & Listed('listed) |-> {
    if ('last < 'min) fail()   
    remove(Active)
    update(Listed('listed - 1))    
    insert(Sold('i))
  }

  "r4" -- list('i) & Active('i,'_,'_) |-> fail()

  "r5" -- list('i) & Sold('i) |-> fail()
  
  "r5" -- bid('i,'_) & not(Active('i,'_,'_)) |-> fail()
      
  "r6" -- sold('i,'_) & not(Active('i,'_,'_)) |-> fail()    
  
  addFact(Listed)(0) 
}

object Demo {
  def main(args: Array[String]) {
    val m = new MaxItemAuctionBidding
    m.PRINT = true
    println("begin monitor")
    m.addEvent('list)("hat", 100)
    m.addEvent('bid)("hat",60)
    m.addEvent('list)("boots", 200)
    m.addEvent('bid)("hat",80)
    m.addEvent('bid)("hat",110) 
    m.addEvent('bid)("boots", 150)
    m.addEvent('sold)("hat")
    m.addEvent('list)("hat")
    println("end monitor")
  }
}