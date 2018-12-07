import scala.io.Source
import java.util.Date
import scala.annotation.tailrec
import java.time.Instant

val filename = "day4.in"
val lines: List[String] = Source.fromFile(filename).getLines.toList.sorted

val guard = "\\[([0-9\\- :]*)\\] Guard #([0-9]*) .*".r
val awake = "\\[([0-9\\- :]*)\\] wakes up".r
val asleep = "\\[([0-9\\- :]*)\\] falls asleep".r

val datePattern = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm")


def getDate(str: String): Date = str match {
    case guard(date, id) => datePattern.parse(date)
    case awake(date) => datePattern.parse(date)
    case asleep(date) =>datePattern.parse(date)
}

def getId(str: String): Option[Int] = str match {
    case guard(date, id) => Some(id.toInt)
    case awake(date) => None
    case asleep(date) => None
}

def sameShift(ido: String): Boolean = ido match {
    case guard(date, id) => false
    case awake(date) => true
    case asleep(date) => true
}

@tailrec
def chunks(events: List[String], accum: List[List[String]]): List[List[String]] = {
    val chunk = events.head :: events.tail.takeWhile(x => sameShift(x))
    val newE = events.tail.dropWhile(x => sameShift(x))
    if(newE.size == 0) {
        accum
    }
    else {
        chunks(newE, chunk :: accum) 
    }
}

def parse(guard: Int, ev: List[String]): (Int, List[(Date, Boolean)]) = (guard, ev.map(x => x match {
    case awake(date) => (datePattern.parse(date), false)
    case asleep(date) => (datePattern.parse(date), true)
}))

def asleepTime(list: List[(Date, Boolean)]): Long = {
    list.grouped(2).foldLeft(0L)((sum, curr) => sum + minDiff(curr(0)._1, curr(1)._1))
}

def minDiff(d1: Date, d2: Date): Long = (d2.getTime() - d1.getTime())/(60*1000) % 60

def minute(d1: Date): Long = d1.getMinutes()

val sep = chunks(lines, List()).map(x => parse(getId(x.head).get, x.tail))

val times = sep.map(x => (x._1, asleepTime(x._2))).groupBy(_._1).map{case (key, value) => key -> value.map(_._2).sum}

val sleeper = times.maxBy(_._2)._1

val sleepertimes = sep.filter(_._1 == sleeper)

def asleepAtMin(min: Int, list: List[(Date, Boolean)]): Int = {
    val drop = list.dropWhile(x => minute(x._1) <= min)
    if(drop.size == 0) {
        0
    }
    else if(drop(0)._2) {
        0
    }
    else {
        1
    }
}

def guardTimesAsleepAt(min: Int, duty: List[(Int, List[(Date, Boolean)])]): Int = {
    duty.map(x => asleepAtMin(min, x._2)).sum
}

val tim = sleepertimes.head
val minutes = (0 to 59).map(m => (m, sleepertimes.map(x => asleepAtMin(m, x._2)).sum))

println(sleeper * minutes.maxBy(_._2)._1) //Part 2

val ids = sep.map(_._1).toSet

val maxMins = ids.map(id => (id, (0 to 59).map(m => (m, guardTimesAsleepAt(m, sep.filter(_._1 == id)))).maxBy(_._2)._1))

val best = maxMins.maxBy(_._2)

println(best._1 * best._2)







