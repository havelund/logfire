package examples.syde.g3_rover.g1_rovercoordination

import rete._

/**
 * Rover coordination.
 *
 * This property relates to the self-organisation of communicating
 * rovers and captures the situation where (at least) one rover is able
 * to communicate with all other (known) rovers. The property states that there
 * exists a leader (rover) who has pinged every other (known) rover and received
 * an acknowledgement. The events ping(from,to) and ack(to,from) indicate that
 * from pinged to and to acknowledged from respectively. The leader does not need
 * to have pinged itself.
 */

// Use this:

class RoverCoordination1 extends Monitor {
  "r1" -- 'ping('r1, 'r2) |-> {
    insert('Node('r1))
    insert('Node('r2))
    insert('Ping('r1, 'r2))
  }

  "r2" -- 'ack('r1, 'r2) |-> {
    insert('Node('r1))
    insert('Node('r2))
  }

  "r3" -- 'Node('r) |-> {
    insert('Cand('r))
    insert('Reach('r, 'r))
  }

  "r4" -- 'Ping('r1, 'r2) & 'ack('r2, 'r1) |-> {
    insert('Reach('r1, 'r2))
    remove('Ping)
  }

  "r5" -- 'end() |-> 'End()

  "r6" -- 'End() & 'Cand('r1) & 'Node('r2) & not('Reach('r1, 'r2)) |-> {
    println(s"no reach from ${'r1.i} to ${'r2.i}")
    remove('Cand)
  }

  "r7" -- 'End() & not('Cand('_)) |-> fail()
}

class RoverCoordination2 extends Monitor {
  val ping, ack, end = event

  var nodes: Set[Int] = Set()
  var pinged: Set[(Int, Int)] = Set()
  var reached: Set[(Int, Int)] = Set()

  def print() {
    println(s"nodes: $nodes")
    println(s"pinged: $pinged")
    println(s"reaches: $reached")
  }

  "r1" -- ping('a, 'b) |-> {
    nodes ++= Set('a.i, 'b.i)
    pinged += (('a.i, 'b.i))
    print()
  }

  "r2" -- ack('b, 'a) |-> {
    nodes ++= Set('b.i, 'a.i)
    if (pinged contains ('a, 'b)) reached += (('a, 'b))
    print()
  }

  "r2" -- end() |-> {
    ensure(nodes.exists(leader => nodes.forall(node => leader == node || reached.contains(leader, node))))
    print()
  }
}

object Evaluate extends MonitorFeeder {
  val ping = 'ping
  val ack = 'ack
  val end = 'end

  val trace1 = List(
    ping(1, 2),
    ack(2, 1),
    ping(2, 3),
    ack(3, 2),
    ping(1, 3),
    ack(3, 1),
    end())

  val trace_1 = List(
    ping(1, 2),
    ack(2, 1),
    ping(2, 3),
    ack(3, 2),
    ping(1, 3),
    //ack(3, 1),
    end())

  def main(args: Array[String]) {
    val m = new RoverCoordination1
    m.PRINT = true
    trace_1 foreach m.addMapEvent
  }
}
