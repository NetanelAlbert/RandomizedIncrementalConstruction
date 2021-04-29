import scala.collection.mutable

class HistoryDAG(val root : Node)
{
    
    def findTriangleNode(p: Point, node: Node) : Node =
    {
        if(node.replacerTriangles.isEmpty) node
        else node.replacerTriangles.get.find(_.triangle.contain(p)).get
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
    
    def getFinalTriangles =
    {
        val finalTriangles = mutable.Seq[Triangle]()
        val nodesQueue = mutable.Queue(root)
        while(nodesQueue.nonEmpty)
        {
            val node = nodesQueue.head
            if(node.isLeaf) finalTriangles :+ node
            else node.replacerTriangles.foreach(_.foreach(nodesQueue += _))
        }
        finalTriangles.toList
    }
    
    def getAdjacentNode(node: Node, match1: Point, match2: Point, unMatch: Point) : Option[Node] =
    {
        if(node.isLeaf && node.triangle.matchFirst2(match1, match2, unMatch)) node
        else node.replacerTriangles.get.foreach
        {
//            child =>
//                val
            for
            {
                ans <- getAdjacentNode(_, match1, match2, unMatch)
    
            }
 
        }
    }
    
    def handleIfBadEdge(node: Node, pOpposite : Point, p1 : Point, p2 : Point)
    {
        val adjacentNode = getAdjacentNode(root, p1, p2, pOpposite)
    }
    
    def handleIfp2p3BadEdge(node: Node)
    {
        node match {
            case Node(Triangle(p1, p2, p3)) => handleIfBadEdge(node, p1, p2, p3)
        }
    }
}
