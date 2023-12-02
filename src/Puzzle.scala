import scala.io.Source

trait Puzzle {
  def run(): Unit

}

object Puzzles {
  val sourceLists = (for {
    i <- 1 to 1
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/aoc2023/src/$i.txt").getLines.toList).toList
  val puzzles = List(
    new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(0)),
  )

}

case class Puzzle1(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val result = (for {
      line <- l
      sumOfDigits = line.find(i => i.isDigit).head.toString + line.reverse.find(i => i.isDigit).head.toString
    } yield sumOfDigits.toInt).sum
    println(s"Result of puzzle 1 is: ${result}")
  }
}

case class Puzzle2(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val DigitFound = """([0-9]|(one|two|three|four|five|six|seven|eight|nine|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin))""".r
    val textToDigitMap = Map("one" -> "1", "two" -> "2", "three" -> "3", "four" -> "4", "five" -> "5", "six" -> "6", "seven" -> "7", "eight" -> "8", "nine" -> "9", "eno" -> "1", "owt" -> "2", "eerht" -> "3", "ruof" -> "4", "evif" -> "5", "xis" -> "6", "neves" -> "7", "thgie" -> "8", "enin" -> "9", "1" -> "1", "2" -> "2", "3" -> "3", "4" -> "4", "5" -> "5", "6" -> "6", "7" -> "7", "8" -> "8", "9" -> "9")
    val result = (for {
      line <- l
      first = textToDigitMap(DigitFound.findFirstIn(line).toList.head.toString())
      second = textToDigitMap(DigitFound.findFirstIn(line.reverse).toList.head.toString())
      sumOfDigits = first + second
    } yield sumOfDigits.toInt).sum
    println(s"Result of puzzle 2 is: ${result}")
  }
}
