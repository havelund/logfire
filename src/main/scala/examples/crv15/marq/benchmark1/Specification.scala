package examples.crv15.marq.benchmark1;

import rete._

/*
 * We consider three entities: voters, parties and candidates. The property states that for every 
 * voter there must exist a party that the voter is a member of, and the voter must rank all 
 * candidates for that party. More precisely, every voter v must be a member of at least one 
 * party p, recorded as member(v,p), and for every candidate c, if c is a candidate for p, 
 * recorded as candidate(c,p), then v must rank c, recorded as rank(v,c,r) where r is the rank.
 * 
 * Events:
 * 
 * - member(v,p)
 * - candidate(c,p)
 * - rank(v,c,r)
 * 
 */

// This is very slow on a large trace. Produces too many facts.

class MRulesButReallySlow extends Monitor {
  val member, candidate, rank = event
  val Member, Option, Obligation = fact

  "r1" -- member('v, 'p) |-> {
    insert(Option('v, 'p))
    insert(Member('v, 'p))
  }

  "r2" -- candidate('c, 'p) & Member('v, 'p) |->
    insert(Obligation('v, 'c, 'p))

  "r3" -- rank('v, 'c, '_) & Obligation('v, 'c, 'p) |->
    remove(Obligation)

  "r4" -- END() & Option('v, 'p) & Obligation('v, '_, 'p) |->
    remove(Option)

  "r5" -- Member('v, '_) & not(Option('v, '_)) |->
    fail("member has not fully ranked a party at end")
}

// It is an example of using the Scala programming constructs
// for writing properties when the logic fails. Scala allows
// us to do the forall-exists-forall quantifier mixtures that
// LogFire cannot handle, and which QEA offers.

class MSSimpleCodeButSlow extends Monitor {
  val member, candidate, rank = event

  type Party = String
  type Candidate = String
  type Member = String

  var members: Set[Member] = Set()
  var memberOf: Set[(Member, Party)] = Set()
  var candidateOf: Set[(Candidate, Party)] = Set()
  var hasRanked: Set[(Member, Candidate)] = Set()

  "r1" -- member('v, 'p) |-> {
    members += 'v.s
    memberOf += (('v.s, 'p.s))
  }

  "r2" -- candidate('c, 'p) |-> {
    candidateOf += (('c.s, 'p.s))
  }

  "r3" -- rank('v, 'c, '_) |-> {
    members += 'v.s
    hasRanked += (('v.s, 'c.s))
  }

  "r4" -- END() |-> {
    ensure("every member must vote for all candidates of at least one party")(
      members.forall {
        case m =>
          memberOf.exists {
            case (m_, p) =>
              m == m_ &&
                candidateOf.forall {
                  case (c, p_) =>
                    p != p_ || hasRanked.contains((m, c))
                }
          }
      })
  }
}

// Optimal implementation:

object Util {
  implicit def updMap(map: Map[String, Set[String]]) = new {
    def +>(a: String, b: String): Map[String, Set[String]] = {
      val set: Set[String] = map.getOrElse(a, Set())
      map + (a -> (set + b))
    }

    def fetch(a: String) = map.getOrElse(a, Set())
  }
}
import Util._

class M extends Monitor {
  val member, candidate, rank = event

  type Party = String
  type Candidate = String
  type Member = String

  var memberOf: Map[Member, Set[Party]] = Map()
  var hasCandidates: Map[Party, Set[Candidate]] = Map()
  var hasRanked: Map[Member, Set[Candidate]] = Map()

  "r1" -- member('v, 'p) |-> {
    memberOf = memberOf +> ('v.s, 'p.s)
  }

  "r2" -- candidate('c, 'p) |-> {
    hasCandidates = hasCandidates +> ('p.s, 'c.s)
  }

  "r3" -- rank('v, 'c, '_) |-> {
    hasRanked = hasRanked +> ('v.s, 'c.s)
  }

  "r4" -- END() |-> {
    val members = memberOf.keySet union hasRanked.keySet

    ensure("every member must vote for all candidates of at least one party")(
      members.forall {
        case m =>
          memberOf.fetch(m).exists {
            case p =>
              hasCandidates.fetch(p).forall {
                case c =>
                  hasRanked.fetch(m) contains c
              }
          }
      })
  }
}

object Evaluate extends MonitorFeeder {
  val member = 'member
  val candidate = 'candidate
  val rank = 'rank
  val end = 'end

  val trace1 = List(
    member("alice", "A"),
    candidate("bob", "A"),
    rank("alice", "bob", 1))

  val trace2 = List(
    member("alice", "A"),
    candidate("bob", "A"),
    member("alice", "B"),
    rank("alice", "bob", 1))

  val trace3 = List(
    member("alice", "A"),
    candidate("bob", "A"),
    candidate("eva", "A"),
    rank("alice", "bob", 2),
    rank("alice", "eva", 1))

  val trace4 = List(
    member("alice", "A"),
    member("bob", "B"),
    candidate("bob", "A"),
    rank("alice", "bob", 1))

  val trace5 = List(
    member("alice", "A"),
    member("alice", "B"),
    candidate("bob", "A"))

  val trace_1 = List(
    member("alice", "A"),
    candidate("bob", "A"))

  val trace_2 = List(
    member("alice", "A"),
    candidate("bob", "A"),
    candidate("eva", "A"),
    rank("alice", "bob", 1))

  val trace_3 = List(
    member("alice", "A"),
    member("bob", "A"),
    candidate("bob", "A"),
    rank("bob", "alice", 1))

  val trace_4 = List(
    member("bob", "A"),
    candidate("allen", "A"),
    rank("bob", "allen", 1),
    rank("tom", "allen", 1))

  def main(args: Array[String]) {
    val m = new M
    m.PRINT = false
    trace_4 foreach m.addMapEvent
    m.terminate()
  }
}
