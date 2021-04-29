import scala.util.Random

object RIC
{
    def ric(points : Seq[Point]) =
    {
        val mixedPoints = Random.shuffle(points)
        val root = Node(Triangle(Point(1, 2), Point(3, 4), Point(5, 6))) // todo create some triangle containing all points
        val historyDAG = new HistoryDAG(root)
        mixedPoints.foreach(historyDAG.addPoint)
        historyDAG.getFinalTriangles
    }
}
