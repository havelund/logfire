package examples.msl

import rete._

case class Tree(name: Symbol, trees: List[Tree])

class TreeMonitor(tree: Tree) extends Monitor {
  private var nameCounter = 0

  private def newName(): String = {
    nameCounter += 1
    "r_" + nameCounter
  }

  generateRules(tree)

  private def generateRules(tree: Tree) {
    val Tree(name, trees) = tree
    val emptyConstraint: Map[Symbol, Any] = Map()
    if (!trees.isEmpty) {
      for (tree <- trees) {
        generateRules(tree)
      }
      var conditions: List[(Boolean, Symbol, Map[Symbol, Any], Symbol)] = {
        val subNames = trees map (_.name)
        for (subName <- subNames) yield (true, subName, emptyConstraint, null)
      }
      val action: Unit => Any = {
        case _ => insert(name())
      }
      addRule(newName(), conditions, action)
    } 
  }
}

object Main {
  def main(args: Array[String]) {
    val s1 = Tree('s1, Nil)
    val s2 = Tree('s2, Nil)
    val s3 = Tree('s3, Nil)
    val a2 = Tree('a2, List(s1, s2))
    val a3 = Tree('a3, List(s1, s2, s3))
    val a1 = Tree('a1, List(a2, a3))

    val m = new TreeMonitor(a1)

    m.PRINT = true
    m.printRules

    m.addMapFact('s1)()
    m.addMapFact('s2)()
    m.addMapFact('s3)()
  }
}
