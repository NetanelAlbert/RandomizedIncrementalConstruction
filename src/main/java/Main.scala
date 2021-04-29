import scala.io.Source

object Main
{
    def main(args: Array[String]): Unit =
    {
        val points = PointsFromFile("teacherSample.txt")
        val trianglesList = RIC.ric(points)
        drawTriangles(trianglesList)
    }

    def PointsFromFile(filename: String) : List[Point] =
    {
        val numReg = """\d+""".r
        val source = Source.fromFile(filename)
        val numbersFromFile = source.getLines.flatMap(numReg.findAllIn).toList.map(_.toInt)
        source.close()
        numbersFromFile match
        {
            case _::coordinatesList =>
                {
                    coordinatesList.grouped(2).map{case List(x, y) => Point(x, y)}.toList
                }
        }
    }

    def drawTriangles(trianglesList: List[Triangle])
    {
        StdDraw.setCanvasSize(1000, 1000)
        StdDraw.setScale(0, 10000)
        trianglesList.foreach(_.draw)
    }
}
