import scala.collection.mutable

class HistoryDAG(val root : Node)
{
    
    def findTriangleNode(p: Point, node: Node) : Node =
    {
        if(node.isLeaf) node
        else
        {
            val n = node.replacerTriangles.get.find(_.triangle.isContain(p)).get
            findTriangleNode(p, n)
        }
    }
    
    def findTriangleNode(p : Point) : Node =
    {
        findTriangleNode(p, root)
    }
    
    def findTriangle(p : Point) : Triangle =
    {
        findTriangleNode(p).triangle
    }
    
    def addPoint(p : Point)
    {
        val containingNode = findTriangleNode(p)
        val Triangle(p1, p2, p3) = containingNode.triangle
        val replacerTriangles = Seq(
            Node(Triangle(p, p1, p2)),
            Node(Triangle(p, p2, p3)),
            Node(Triangle(p, p3, p1))
        )
        containingNode.replacerTriangles = Some(replacerTriangles)
        replacerTriangles.foreach(handleIfp2p3BadEdge)
    }
    
    def getFinalTriangles : List[Triangle] =
    {
        val finalTriangles = mutable.ListBuffer[Triangle]()
        val nodesQueue = mutable.Queue(root)
        while(nodesQueue.nonEmpty)
        {
            val node = nodesQueue.dequeue
            if(node.isLeaf) finalTriangles += node.triangle
            else node.replacerTriangles.foreach(_.foreach(nodesQueue += _))
        }
        finalTriangles.toList
    }
    
    def getAdjacentNode(node: Node, match1: Point, match2: Point, unMatch: Point) : Option[Node] =
    {
        if(node.isLeaf && node.triangle.matchFirst2(match1, match2, unMatch)) return Some(node)
        else node.replacerTriangles.getOrElse(Seq()).foreach
        {
            child =>
                val ans = getAdjacentNode(child, match1, match2, unMatch)
                if(ans.isDefined) return ans
        }
        
        None
    }
    
    def handleIfBadEdge(node: Node, pOpposite : Point, p1 : Point, p2 : Point)
    {
        val adjacentNodeOption = getAdjacentNode(root, p1, p2, pOpposite)
        adjacentNodeOption match
        {
            case Some(adjacentNode @ Node(adjacentTriangle @ Triangle(op1, op2, op3))) =>
            {
                val otherPOpposite = Seq(op1, op2, op3).find(p => p != p1 && p != p2).get
                if(node.triangle.isCycleContain(otherPOpposite)
                    || adjacentTriangle.isCycleContain(pOpposite))
                {
                    val replacerTriangles = Seq(
                        Node(Triangle(pOpposite, otherPOpposite, p1)),
                        Node(Triangle(pOpposite, otherPOpposite, p2))
                    )
                    node.replacerTriangles = Some(replacerTriangles)
                    adjacentNode.replacerTriangles = Some(replacerTriangles)
                    replacerTriangles.foreach(node => handleIfp2p3BadEdge(node))
                }
            }

            case None => // do nothing (outer triangle)
        }
    }
    
    def handleIfp2p3BadEdge(node: Node)
    {
        node match
        {
            case Node(Triangle(p1, p2, p3)) => handleIfBadEdge(node, p1, p2, p3)
        }
    }
}
