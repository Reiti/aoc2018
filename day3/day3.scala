import scala.io.Source

val filename = "day3.in"
val lines: List[Sheet] = Source.fromFile(filename).getLines.map(string2Sheet).toList

case class Sheet(id: Int, x1: Int, y1: Int, x2: Int, y2: Int)

def string2Sheet(line: String) = {
    val patt = "[0-9]+".r
    val ints = patt.findAllIn(line).toList.map(_.toInt)
    val id = ints(0)
    val x1 = ints(1)
    val y1 = ints(2)
    val x_s = ints(3)
    val y_s = ints(4)

    Sheet(id, x1, y1, x1 + x_s, y1 + y_s)
}

val x_m = lines.map(_.x2).max
val y_m = lines.map(_.y2).max

def inside(s: Sheet, x: Int, y: Int): Boolean = x >= s.x1 && x < s.x2 && y >= s.y1 && y < s.y2

val ret = (0 to x_m).map{x => 
    (0 to y_m).map {
        y => (lines.map(inside(_, x, y)).count(_ == true))
    }
}

println(ret.flatten.filter(_ > 1).size) //Part1 XD

def check(s: Sheet, lattice: Seq[Seq[Int]]): Boolean = {
    (s.x1 to s.x2).map { x =>
        (s.y1 to s.y2).map { y =>
            lattice(x)(y)
        }
    }.flatten.count(_ > 1) == 0
}


println(lines.find(check(_, ret)).get.id)