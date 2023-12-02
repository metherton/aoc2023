import scala.io.Source

trait Puzzle {
  def run(): Unit

}

object Puzzles {
  val sourceLists = (for {
    i <- 1 to 2
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/aoc2023/src/$i.txt").getLines.toList).toList
  val puzzles = List(
    new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(0)),
    new Puzzle3(sourceLists(1)),
    new Puzzle4(sourceLists(1)),
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

case class Puzzle3(value: List[String]) extends Puzzle {

//  def createMapLine(str: String): Map[String, Map[String, Int]] = {
//    val gameNumberParts = str.split(":").map(_.trim).toList
//    val gameDayAttempts = gameNumberParts.reverse.head.split(";").toList
//    val gameNumber = gameNumberParts.head.split(" ").toList.reverse.head
//    val doMap = (for {
//      attempt <- gameDayAttempts
//      efforts = attempt.split(", ").toList.map(x => x.trim.split(" "))
//      //flattenedEfforts = efforts.flatten
//      effortsMap = efforts.map(x => (x(1), x(0)))
//    } yield effortsMap).flatten
//
//    val dayMap = doMap.groupBy(_._1).map(s => (s._1, s._2.map(t => t._2.toInt).sum))
//    Map(gameNumber -> dayMap)
//  }
  def createMapLine(str: String): Map[String, List[(String, String)]] = {
    val gameNumberParts = str.split(":").map(_.trim).toList
    val gameDayAttempts = gameNumberParts.reverse.head.split(";").toList
    val gameNumber = gameNumberParts.head.split(" ").toList.reverse.head
    val doMap = (for {
      attempt <- gameDayAttempts
      efforts = attempt.split(", ").toList.map(x => x.trim.split(" "))
      //flattenedEfforts = efforts.flatten
      effortsMap = efforts.map(x => (x(1),x(0)))
    } yield effortsMap).flatten

    val dayMap = doMap.groupBy(_._1).map(s => (s._1, s._2.map(t => t._2.toInt).sum))
    Map(gameNumber -> doMap)
  }

  override def run(): Unit = {
    val result = for {
      line <- value
      mapline <- createMapLine(line)
    } yield mapline
    println(result)
    val lefts = result.filter(el => el._2.forall(e => {
      e._1 match {
        case "red" => e._2.toInt <= 12
        case "green" => e._2.toInt <= 13
        case "blue" => e._2.toInt <= 14
        case _ => true
      }
    }))
      println(s"Result of puzzle 2 is: ${lefts.map(s => s._1.toInt).sum}")
  }
}

case class Puzzle4(value: List[String]) extends Puzzle {

  //  def createMapLine(str: String): Map[String, Map[String, Int]] = {
  //    val gameNumberParts = str.split(":").map(_.trim).toList
  //    val gameDayAttempts = gameNumberParts.reverse.head.split(";").toList
  //    val gameNumber = gameNumberParts.head.split(" ").toList.reverse.head
  //    val doMap = (for {
  //      attempt <- gameDayAttempts
  //      efforts = attempt.split(", ").toList.map(x => x.trim.split(" "))
  //      //flattenedEfforts = efforts.flatten
  //      effortsMap = efforts.map(x => (x(1), x(0)))
  //    } yield effortsMap).flatten
  //
  //    val dayMap = doMap.groupBy(_._1).map(s => (s._1, s._2.map(t => t._2.toInt).sum))
  //    Map(gameNumber -> dayMap)
  //  }
  def createMapLine(str: String): Map[String, Map[String, Int]] = {
    val gameNumberParts = str.split(":").map(_.trim).toList
    val gameDayAttempts = gameNumberParts.reverse.head.split(";").toList
    val gameNumber = gameNumberParts.head.split(" ").toList.reverse.head
    val doMap = (for {
      attempt <- gameDayAttempts
      efforts <- attempt.split(", ").toList.map(x => x.trim.split(" "))
      effortsMap = (efforts(0), efforts(1))
    } yield effortsMap)
    val dayMap = doMap.groupBy(_._2).map(x => (x._1, x._2.map(y => y._1.toInt).max))
    Map(gameNumber -> dayMap)
  }

  override def run(): Unit = {
    val result = for {
      line <- value
      mapline <- createMapLine(line)
    } yield mapline
    println(result)
//    val lefts = result.filter(el => el._2.forall(e => {
//      e._1 match {
//        case "red" => e._2.toInt <= 12
//        case "green" => e._2.toInt <= 13
//        case "blue" => e._2.toInt <= 14
//        case _ => true
//      }
//    }))
    println(s"Result of puzzle 4 is: ${result.map(s => s._2.values.toList.map(_.toInt).product).sum}")
  }
}