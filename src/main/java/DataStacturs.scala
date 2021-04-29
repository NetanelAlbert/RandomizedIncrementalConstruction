case class Node(triangle : Triangle)
{
    var replacerTriangles : Option[Seq[Node]] = None
    def isLeaf = replacerTriangles.isEmpty
}

case class Point(x : Double, y : Double)

case class Triangle(p1 : Point, p2 : Point, p3 : Point)
{
    def matchFirst2(match1: Point, match2: Point, unMatch: Point) : Boolean =
    {
        (p1 != unMatch && p2 == match1 && p3 == match2) ||
        (p1 != unMatch && p3 == match1 && p2 == match2) ||
        (p2 != unMatch && p1 == match1 && p3 == match2) ||
        (p2 != unMatch && p3 == match1 && p1 == match2) ||
        (p3 != unMatch && p2 == match1 && p1 == match2) ||
        (p3 != unMatch && p1 == match1 && p2 == match2)
    }
    
    def isContain(p : Point) : Boolean =
    {
        val as_x = p.x - p1.x
        val as_y = p.y - p1.y
    
        val s_ab = (p2.x - p1.x) * as_y - (p2.y - p1.y) * as_x > 0
    
        if ((p3.x - p1.x) * as_y - (p3.y - p1.y) * as_x > 0 == s_ab) false
        else if ((p3.x - p2.x) * (p.y - p2.y) - (p3.y - p2.y) * (p.x - p2.x) > 0 != s_ab) false
        else true
    }
    
    def g(x : Double,y : Double,cx : Double,cy : Double,r : Double) : Boolean =
    {
        Math.sqrt((x-cx)*(x-cx)+(y-cy)*(y-cy))<r
    }
    
    def isCycleContain(p : Point) : Boolean =
    {
        val m1 = (p1.x - p2.x) / (p2.y - p1.y)
        val m2 = (p1.x - p3.x) / (p3.y - p1.y)
        val b1 = ((p1.y + p2.y) / 2) - m1 * (p1.x + p2.x) / 2
        val b2 = ((p1.y + p3.y) / 2) - m2 * (p1.x + p3.x) / 2
        val xx = (b2 - b1) / (m1 - m2)
        val yy = m1 * xx + b1
        g(p.x, p.y, xx, yy, Math.sqrt((xx - p1.x) * (xx - p1.x) + (yy - p1.y) * (yy - p1.y)))
    }
    
    def draw
    {
        StdDraw.line(p1.x, p1.y, p2.x, p2.y)
        StdDraw.line(p2.x, p2.y, p3.x, p3.y)
        StdDraw.line(p3.x, p3.y, p1.x, p1.y)
    }
}
