case class Node(triangle : Triangle)
{
    var replacerTriangles : Option[Seq[Node]] = None
    def isLeaf = replacerTriangles.isEmpty
}
case class Point(x : Double, y : Double)
case class Triangle(p1 : Point, p2 : Point, p3 : Point)
{
    def contain(p : Point) : Boolean =
    {
        true // todo implement
    }
}
