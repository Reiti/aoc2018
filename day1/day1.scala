import scala.io.Source

val filename = "day1.in"
val lines = Source.fromFile(filename).getLines.map(_.toInt).toList

println(lines.sum) //Part 1

val loop: Stream[Int] = lines.toStream #::: loop
def firstTwice(occurred: Set[Int], nums: Stream[Int], curr: Int): Int = if (occurred(curr)) curr else firstTwice(occurred + curr, nums.tail, curr + nums.head)
println(firstTwice(Set(), loop, 0)) //Part 2

