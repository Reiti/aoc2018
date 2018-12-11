import scala.io.Source
import scala.annotation.tailrec

val filename = "day10.in"
val input = Source.fromFile(filename).getLines.toList

val reg = "position=<([\\- \\d]+),([\\- \\d]+)> velocity=<([\\- \\d]+), ([\\- \\d]+)>".r

case class Pixel(x: Int, y: Int, xv: Int, yv: Int)

val parsed = input.map(x => x match {
  case reg(x, y, xv, yv) => Pixel(x.trim.toInt, y.trim.toInt, xv.trim.toInt, yv.trim.toInt)
  case _ => Pixel(0, 0, 0, 0)
})

def rec(l: List[Pixel]): ((Int, Int), (Int, Int)) = {
  val x1 = l.minBy(_.x).x
  val y1 = l.maxBy(_.y).y
  val x2 = l.maxBy(_.x).x
  val y2 = l.minBy(_.y).y
  ((x1, y1), (x2, y2))
}

def height(r: ((Int, Int), (Int, Int))): Int = Math.abs(r._1._2 - r._2._2)
def width(r: ((Int, Int), (Int, Int))): Int = Math.abs(r._2._1 - r._1._1)

def diag(rec: ((Int, Int), (Int, Int))): Int = {
  val dx = width(rec)
  val dy = height(rec)
  
  println(dx + " - " + dy)
  Math.sqrt(dx*dx + dy*dy).toInt
}

def advance(img: List[Pixel]): List[Pixel] = img.map(p => p match {
  case Pixel(x, y, xv, yv) => Pixel(x + xv, y + yv, xv, yv)
})

def within(p: Pixel, r: ((Int, Int), (Int, Int))): Boolean = {
  val x1 = r._1._1
  val y1 = r._1._2
  val x2 = r._2._1
  val y2 = r._2._2
  
  p.x > x1 && p.x < x2 && p.y < y1 && p.y > y2
}
def printrec(img: List[Pixel], r: ((Int, Int), (Int, Int))): Unit = {
  val x1 = r._1._1
  val y1 = r._1._2
  val x2 = r._2._1
  val y2 = r._2._2
  
  val normalized = img.map(p => p match {
    case Pixel(x, y, xv, yv) => (x - x1, y - y1)
  })

  
  val xf = normalized.minBy(_._1)._1
  val xt = normalized.maxBy(_._1)._1
  val yf = normalized.maxBy(_._2)._2
  val yt = normalized.minBy(_._2)._2
  
  val msg = (yt to yf).map(yc => {
    (yc, (xf to xt).map(xc => if(normalized.exists(p => p._1 == xc && p._2 == yc)) "#" else ".").mkString)
  })
  
  
  msg.foreach(x => println(x._2)) //Part 1
  
  
}

@tailrec
private def find(img: List[Pixel], count: Int): Unit = {
  val r = rec(img)
  val ad = advance(img)
  val nc = count+1
  if(height(r) <= 10) {
    printrec(img, r)
    println(count) //Part 2
  }else {
    find(ad, nc)  
  }
}

find(parsed, 0) 

