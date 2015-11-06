object Dot {
  class RefEq[A <: AnyRef](val value: A) {
    override def equals(rhs: Any) = rhs match {
      case r: RefEq[_] => this.value eq r.value
      case _ => false
    }
    override def hashCode =
      System.identityHashCode(value)
  }

  class IdGen {
    private[this] var _id = 0

    def nextId(): Int = {
      _id += 1
      _id
    }
  }

  def compile(sf: SignalFunction[_, _], idGen: IdGen): (Node, Node, Seq[Link]) = {
    sf match {
      case IdSF() =>
        val n = SimpleNode(idGen.nextId(), "id")
        (n, n, Seq.empty)
      case FunctionSF(_) =>
        val n = SimpleNode(idGen.nextId(), "function")
        (n, n, Seq.empty)
      case ConstSF(c) =>
        val n = SimpleNode(idGen.nextId(), s"const ${c}")
        (n, n, Seq.empty)
      case NamedSF(name, sf1) =>
        val (h, t, l) = compile(sf1, idGen)
        val g = SubGraph(idGen.nextId(), name, h, t, l)
        (g, g, Seq.empty)
      case StatefulSF(sf1, init) =>
        val (h, t, l) = compile(sf1, idGen)
        val g = SubGraph(idGen.nextId(), s"stateful(init=${init})", h, t, l)
        (g, g, Seq(Link(g, g)))
      case ComposeSF(sf1, sf2) =>
        val (h1, t1, l1) = compile(sf1, idGen)
        val (h2, t2, l2) = compile(sf2, idGen)
        (h1, t2, l1 ++ l2 ++ Seq(Link(t1, h2)))
      case CombineSF(sf1, sf2) =>
        val (h1, t1, l1) = compile(sf1, idGen)
        val (h2, t2, l2) = compile(sf2, idGen)
        val h = SimpleNode(idGen.nextId(), "&&&")
        val t = SimpleNode(idGen.nextId(), "&&&")
        (h, t, l1 ++ l2 ++ Seq(Link(h, h1), Link(h, h2), Link(t1, t), Link(t2, t)))
      case ParallelSF(sf1, sf2) =>
        val (h1, t1, l1) = compile(sf1, idGen)
        val (h2, t2, l2) = compile(sf2, idGen)
        val h = SimpleNode(idGen.nextId(), "***")
        val t = SimpleNode(idGen.nextId(), "***")
        (h, t, l1 ++ l2 ++ Seq(Link(h, h1), Link(h, h2), Link(t1, t), Link(t2, t)))
    }
  }

  def toSource(data: (Node, Node, Seq[Link]), header: String = "digraph", attrs: Seq[String] = Seq.empty): String = {
    val (start, end, links) = data
    val nodes = (Seq(start, end) ++ links.flatMap { l => Seq(l.l, l.r) }).distinct

    val nodeSources = nodes.map {
      case n @ SimpleNode(_, label) =>
        s"""${n.id} [label="${label}"]"""
      case n @ SubGraph(_, label, start, end, links) =>
        toSource((start, end, links), s"subgraph ${n.id}", Seq(s"""label="${label}""""))
    }
    val linkSources = links.map {
      case Link(l, r) =>
        s"${l.srcId} -> ${r.destId}"
    }

    (Seq(s"${header} {") ++ attrs ++ nodeSources ++ linkSources ++ Seq("}")).mkString("\n")
  }

  case class Link(l: Node, r: Node)

  class Node(val id: String, val label: String) {
    def srcId: String = this.id
    def destId: String = this.id
  }
  case class SimpleNode(idBase: Int, override val label: String) extends Node(s"node_${idBase}", label)
  case class SubGraph(idBase: Int, override val label: String, start: Node, end: Node, link: Seq[Link]) extends Node(s"cluster_${idBase}", label) {
    override def srcId = unwrapEnd.id
    override def destId = unwrapStart.id

    def unwrapStart: SimpleNode = start match {
      case sg: SubGraph => sg.unwrapStart
      case s: SimpleNode => s
    }

    def unwrapEnd: SimpleNode = end match {
      case sg: SubGraph => sg.unwrapEnd
      case s: SimpleNode => s
    }
  }
}
