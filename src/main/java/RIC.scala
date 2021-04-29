import scala.util.Random

object RIC
        {
        def ric(points:Seq[Point]):List[Triangle]=
        {
        val mixedPoints=Random.shuffle(points)
        val root=Node(Triangle(Point(0,0),Point(10000,0),Point(5000,8660))) // given in the task
        val historyDAG=new HistoryDAG(root)
        mixedPoints.foreach(historyDAG.addPoint)

        historyDAG.getFinalTriangles
        }
        }
