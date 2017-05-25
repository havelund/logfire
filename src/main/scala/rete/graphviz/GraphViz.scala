package rete.graphviz

private trait GraphOperations {
  var stmts: List[Stmt]

  def newSubgraph(optId: Option[String] = None): SubGraph = {
    val subGraph = SubGraph(optId, Nil)
    val stmt = SubGraphStmt(subGraph)
    stmts ::= stmt
    subGraph
  }

  def addNode(id: String, attributes: List[(String, String)] = Nil) {
    val nodeId = NodeId(id: String, None)
    val attrs = for ((id, attr) <- attributes) yield (id -> Some(attr))
    val stmt = NodeStmt(nodeId: NodeId, AttrList(List(Some(AList(attrs)))))
    stmts = stmts ++ List(stmt)
  }

  def addEdge(sourceId: String, portid: Option[String], targetId: String, attributes: List[(String,String)]) {
    val port = portid match {
      case None => None
      case Some(pid) => Some(PortWithId(pid, None))
    }
    val source = NodeIdSource(NodeId(sourceId, port))
    val edgeRHS = EdgeRHS(List((Arrow -> NodeIdSource(NodeId(targetId, None)))))
    val attrs = for ((id, attr) <- attributes) yield (id -> Some(attr))
    val attrList = AttrList(List(Some(AList(attrs))))
    val stmt = EdgeStmt(source, edgeRHS, attrList)
    stmts = stmts ++ List(stmt)
  }
}

// graph :	[ strict ] (graph | digraph) [ ID ] '{' stmt_list '}'
// stmt_list : [ stmt [ ';' ] [ stmt_list ] ]

//trait GraphType
//case object Graph extends GraphType {
//  override def toString = "graph"
//}
//case object Digraph extends GraphType {
//  override def toString = "digraph"
//}

private case class Graph(strict: Boolean = false, digraph: Boolean = true, optId: Option[String] = None, var stmts: List[Stmt] = Nil)
  extends GraphOperations {
  override def toString = {
    var result = ""
    if (strict) result += "strict "
    result += (if (digraph) "digraph" else "graph") + " "
    optId match {
      case None =>
      case Some(id) => result += id + " "
    }
    result += "{" + Format.nl
    Format.level += 1
    for (stmt <- stmts) {
      result += Format.indent + stmt.toString + ";" + Format.nl
    }
    Format.level -= 1
    result += "}"
    result
  }
}

// stmt : 
//   node_stmt
// | edge_stmt
// | attr_stmt
// | ID '=' ID
// | subgraph

private trait Stmt

// node_stmt : node_id [ attr_list ]

private case class NodeStmt(nodeId: NodeId, attrs: AttrList) extends Stmt {
  override def toString = nodeId + " " + attrs
}

// edge_stmt : (node_id | subgraph) edgeRHS [ attr_list ]

private case class EdgeStmt(source: NodeIdOrSubGraph, edgeRHS: EdgeRHS, attrs: AttrList) extends Stmt {
  override def toString = source + " " + edgeRHS + " " + attrs
}

// attr_stmt : (graph | node | edge) attr_list

private case class AttrStmt(attrKind: AttrKind, attrs: AttrList) extends Stmt {
  override def toString = attrKind + " " + attrs
}

private case class EqualStmt(id1: String, id2: String) extends Stmt {
  override def toString = id1 + " = " + id2
}

private case class SubGraphStmt(subgraph: SubGraph) extends Stmt {
  override def toString = subgraph.toString()
}

// node_id : ID [ port ]

private case class NodeId(id: String, port: Option[Port]) {
  override def toString =
    id + {
      port match {
        case None => ""
        case Some(thePort) => " " + thePort
      }
    }
}

// port : 
//   ':' ID [ ':' compass_pt ]
// | ':' compass_pt

private trait Port
private case class PortWithId(id: String, compassPt: Option[CompassPt]) extends Port {
  override def toString =
    ": " + id + {
      compassPt match {
        case None => ""
        case Some(theCompassPt) => " : " + theCompassPt
      }
    }
}
private case class PortWithNoId(compassPt: CompassPt) extends Port {
  override def toString = compassPt.toString
}

// compass_pt : (n | ne | e | se | s | sw | w | nw | c | _)

private trait CompassPt
private case object dir_N extends CompassPt { override def toString = "n" }
private case object dir_NE extends CompassPt { override def toString = "ne" }
private case object dir_E extends CompassPt { override def toString = "e" }
private case object dir_SE extends CompassPt { override def toString = "se" }
private case object dir_S extends CompassPt { override def toString = "s" }
private case object dir_SW extends CompassPt { override def toString = "sw" }
private case object dir_W extends CompassPt { override def toString = "w" }
private case object dir_NW extends CompassPt { override def toString = "nw" }
private case object dir_C extends CompassPt { override def toString = "c" }
private case object dir_DontCare extends CompassPt { override def toString = "_" }

// attr_list : '[' [ a_list ] ']' [ attr_list ]

private case class AttrList(attrList: List[Option[AList]]) {
  override def toString = {
    var result = ""
    for (alist <- attrList) {
      val aliststr = alist match {
        case None => ""
        case Some(theAlist) => theAlist
      }
      result += "[" + aliststr + "]"
    }
    result
  }
}

// a_list : ID [ '=' ID ] [ ',' ] [ a_list ]

private case class AList(alist: List[(String, Option[String])]) {
  override def toString = {
    var result = ""
    var comma = ""
    for ((id, optid) <- alist) {
      result += comma + id +
        (optid match {
          case None => ""
          case Some(theId) => "=" + theId
        })
      comma = ", "
    }
    result
  }
}

// ... (graph | node | edge) ...

private trait AttrKind
private case object graphAttr extends AttrKind {
  override def toString = "graph"
}
private case object nodeAttr extends AttrKind {
  override def toString = "node"
}
private case object edgeAttr extends AttrKind {
  override def toString = "edge"
}

// subgraph : [ subgraph [ ID ] ] '{' stmt_list '}'

private case class SubGraph(optid: Option[String], var stmts: List[Stmt]) extends GraphOperations {
  override def toString = {
    var result = "subgraph" // we always print it
    optid match {
      case None =>
      case Some(theId) => result += " " + theId
    }
    result += "{"
    Format.level += 1
    for (stmt <- stmts) {
      result += Format.indent + stmt
    }
    Format.level -= 1
    result += "}"
    result
  }
}

// edgeRHS : edgeop (node_id | subgraph) [ edgeRHS ]

private case class EdgeRHS(edgeRHS: List[(EdgeOp, NodeIdOrSubGraph)]) {
  override def toString = {
    var result = ""
    for ((edgeOp, nodeIdOrSubGraph) <- edgeRHS) {
      result += edgeOp + " " + nodeIdOrSubGraph
    }
    result
  }
}

private trait NodeIdOrSubGraph
private case class NodeIdSource(nodeId: NodeId) extends NodeIdOrSubGraph {
  override def toString = nodeId.toString()
}
private case class SubgraphSource(subgraph: SubGraph) extends NodeIdOrSubGraph {
  override def toString = subgraph.toString()
}

private trait EdgeOp
private case object Arrow extends EdgeOp {
  override def toString = "->"
}
private case object Line extends EdgeOp {
  override def toString = "--"
}

private object Format {
  var level: Int = 0
  val tab = "  "
  val nl = "\n"

  def indent: String = tab * level
}
