package rete.graphviz

import java.io._
import Util._

import scala.language.implicitConversions

private object Util {
  val DEBUG = false

  def debug(str: String) {
    if (DEBUG) println("===> " + str)
  }

  var counter = 0

  def newName: String = {
    counter += 1
    "s" + counter
  }

  implicit def myExists[A](c: Iterable[A]) = new {
    def Exists(p: PartialFunction[A, Boolean]): Boolean =
      c exists (p orElse { case _ => false })

    def NotExists(p: PartialFunction[A, Boolean]): Boolean =
      !(c exists (p orElse { case _ => false }))
  }

  def open(totalName: String): PrintWriter = new PrintWriter(new File(totalName))
}

private class EqMap[A <: AnyRef, B] {
  var map: List[(A, B)] = Nil

  def apply(a: A): B = {
    get(a) match {
      case None => throw new RuntimeException("look in EqMap failed")
      case Some(b) => b
    }
  }

  def get(a: A): Option[B] =
    (map find { case (x, _) => x eq a }) match {
      case None => None
      case Some((_, b)) => Some(b)
    }

  def contains(a: A): Boolean =
    get(a) match {
      case None => false
      case Some(_) => true
    }

  def +=(pair: (A, B)) {
    val (a, b) = pair
    map = map filterNot { case (x, y) => x eq a }
    map ::= (a, b)
  }

  override def toString: String = map.mkString("[", ",", "]")
}

private case class NodeInfo(name: String, var attributes: List[(String, String)]) {
  override def toString: String = name + " " + attributes.mkString("[", ",", "]")
}

private case class EdgeInfo(var attributes: List[(String, String)]) {
  override def toString: String = attributes.mkString("[", ",", "]")
}

private[rete] class Config {
  type Node = AnyRef

  def addAttrs(node: Node, attributes: (String, String)*) {
    assert(!node.isInstanceOf[(_, _)])
    addNode(node, attributes.toList)
  }

  def addFields(node: Node, fields: (Symbol, Any)*) {
    addFields(node, fields.toList)
  }

  def addFields(node: Node, fields: List[(Symbol, Any)]) {
    assert(!node.isInstanceOf[(_, _)])
    val attributes = record(fields)
    addNode(node, attributes)
  }

  def addCells(node: Node, symbols: Symbol*) {
    assert(!node.isInstanceOf[Symbol])
    val oldSymbols: Set[Symbol] = cells.get(node) match {
      case None => Set()
      case Some(symbolSet) => symbolSet
    }
    val newSymbols = symbols.toSet filter (!oldSymbols.contains(_))
    if (!newSymbols.isEmpty) {
      cells += (node -> (oldSymbols union newSymbols))
      val label = newSymbols map (_.toString.substring(1)) map
        (s => "<" + s + ">" + s) mkString ("|")
      addAttrs(node, "label" -> label)
    }
  }

  def shapeNode(node: Node, shape: String) {
    addAttrs(node, "shape" -> shape)
  }

  def colorNode(node: Node, color: String, filled: Boolean = false) {
    if (filled)
      addAttrs(node, "style" -> "filled", "fillcolor" -> color)
    else
      addAttrs(node, "color" -> color)
  }

  def addEdge(source: Node, target: Visual, attributes: (String, String)*) {
    addEdge(source, None, target, attributes: _*)
  }

  def addEdge(source: Node, port: Symbol, target: Visual, attributes: (String, String)*) {
    addCells(source, port)
    addEdge(source, Some(port.toString().substring(1)), target, attributes: _*)
  }

  def addEdges(source: Node, targets: Iterable[Visual], attributes: (String, String)*) {
    for (target <- targets)
      addEdge(source, target, attributes: _*)
  }

  def addEdges(source: Node, port: Symbol, targets: Iterable[Visual], attributes: (String, String)*) {
    for (target <- targets)
      addEdge(source, port, target, attributes: _*)
  }

  def dump(fileName: String) {
    val graph = makeGraph
    val file = open(fileName)
    file.println(graph)
    file.close()
  }

  override def toString: String = {
    var result = ""
    for ((node, info) <- nodes.map) result += node + " -> " + info + "\n"
    for (edge <- edges) result += edge.toString() + "\n"
    result
  }

  private val done: EqMap[Node, Boolean] = new EqMap()
  private val nodes: EqMap[Node, NodeInfo] = new EqMap()
  private var edges: List[(Node, Option[String], Node, EdgeInfo)] = Nil
  private var cells: EqMap[Node, Set[Symbol]] = new EqMap()

  private[rete] def reached(node: Node): Boolean = done contains node

  private[rete] def reach(node: Node) { done += (node -> true) }

  private def addNode(node: Node, attributes: List[(String, String)]) {
    nodes.get(node) match {
      case None =>
        val attributesBegin = List("shape" -> "record", "label" -> ("--- " + node.getClass.getSimpleName + " ---"))
        nodes += (node -> NodeInfo(newName, updateAttributes(attributesBegin, attributes)))
      case Some(NodeInfo(name, attributesBefore)) =>
        nodes += (node -> NodeInfo(name, updateAttributes(attributesBefore, attributes)))
    }
  }

  private def addEdge(source: Node, port: Option[String], target: Visual, attributes: (String, String)*) {
    if (target != null) {
      addAttrs(source)
      addAttrs(target)
      edges ::= ((source, port, target, EdgeInfo(attributes.toList)))
      target.graph(this)
    }
  }

  private def updateAttributes(oldAttributes: List[(String, String)], newAttributes: List[(String, String)]): List[(String, String)] = {
    var combinedAttributes = oldAttributes.toMap
    for ((key, value) <- newAttributes) {
      if (key == "label") {
        oldAttributes find {
          case (k, _) => k == "label"
        } match {
          case None =>
            combinedAttributes += (key -> value)
          case Some((k, v)) =>
            combinedAttributes += (k -> (v + "|" + value))
        }
      } else {
        combinedAttributes += (key -> value)
      }
    }
    combinedAttributes.toList
  }

  private def record(fields: List[(Symbol, Any)]): List[(String, String)] = {
    List("label" -> table(fields))
  }

  private def table(rows: List[(Symbol, Any)]): String = {
    var result = ""
    var sep = ""
    for ((id, value) <- rows) {
      result += sep + "{" + id.toString().substring(1) + "|" + represent(value) + "}"
      sep = "|"
    }
    result
  }

  private def represent(value: Any): String =
    value match {
      case list: List[_] => list.mkString("[", ",", "]")
      case set: Set[_] => set.mkString("\\{", ",", "\\}")
      case null => "null"
      case _ => value.toString
    }

  private def makeGraph: Graph = {
    val graph = Graph()
    for ((node, NodeInfo(name, attrs)) <- nodes.map) {
      graph.addNode(name, finalizeLabels(attrs))
    }
    for ((source, port, target, info) <- edges) {
      val sourceName = nodes(source).name
      val targetName = nodes(target).name
      graph.addEdge(sourceName, port, targetName, info.attributes)
    }
    graph
  }

  private def finalizeLabels(attributes: List[(String, String)]): List[(String, String)] = {
    var attributeMap = attributes.toMap
    attributeMap.get("label") match {
      case None => attributeMap += ("label" -> "\"\"")
      case Some(value) => attributeMap += ("label" -> ("\"{" + value + "}\""))
    }
    attributeMap.toList
  }
}

private[rete] trait Visual {
  private var file: String = null 

  private[rete] def graph(config: Config) {
    if (!(config.reached(this))) {
      config.reach(this)
      toGraph(config)
    }
  }

  /**
   * Sets the dot file to which graphs showing the internal RETE data structures will be written.
   *
   * @param file the .dot file to which the graph will be written. The file should end in <code>.dot</code>.
   *             The file can be opened with <a href="http://www.graphviz.org">Graphviz</a> after
   *             <code>draw()</code> has been called.
   */

  def dotFile(file: String) {
    this.file = file
  }

  /**
   * Draws the internal RETE data structure and writes it as a .dot (Graphviz) representation to the file
   * set by <code>dotFile(String)</code>. Such drawing of the internal RETE data structure is useful for
   * understanding how RETE works. It is meant to be for debugging purposes only.
   */

  def draw() {
    val config = new Config
    graph(config)
    config.dump(file)
  }

  /**
   * Draws the internal RETE data structure and writes it as a .dot (Graphviz) representation to the file
   * provided as parameter. A call of this function is equivalent to: <code>dotFile(file);draw()</code>.
   *
   * @param file
   */

  def draw(file: String) {
    dotFile(file)
    draw()
  }

  private[rete] def toGraph(config: Config)
}
