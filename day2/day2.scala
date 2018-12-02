import scala.io.Source

val filename = "day2.in"
val lines = Source.fromFile(filename).getLines.toList

def containsN(in: String, n: Int): Boolean = in.split("").groupBy(_.charAt(0)).values.exists(_.size == n)

val two = lines.filter(containsN(_, 2)).size
val three = lines.filter(containsN(_, 3)).size

println(two*three) //Part 1

def difference(word1: String, word2: String): Int = (word1.split("") zip word2.split("")).filter(x => x._1 != x._2).size 

def findOneDifferent(word: String, words: List[String]): Option[String] = words.find(w => difference(w, word) == 1)

def common(word1: String, word2: String): String = (word1.split("") zip word2.split("")).filter(x => x._1 == x._2).map(_._1).foldLeft("")(_ + _)

def matchFinder(list: List[String]): String = findOneDifferent(list.head, list.tail) match {
  case Some(dif) => common(list.head, dif)
  case None => matchFinder(list.tail)
}

println(matchFinder(lines)) //Part 2

