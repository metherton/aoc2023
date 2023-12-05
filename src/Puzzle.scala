import scala.io.Source

trait Puzzle {
  def run(): Unit

}

object Puzzles {
  val sourceLists = (for {
    i <- 1 to 4
  } yield Source.fromFile(s"/Users/dt10dw/Developer/ING/code/backend/aoc2023/src/$i.txt").getLines.toList).toList
  val puzzles = List(
    new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(0)),
    new Puzzle3(sourceLists(1)),
    new Puzzle4(sourceLists(1)),
    new Puzzle5(sourceLists(2)),
    new Puzzle6(sourceLists(2)),
    new Puzzle7(sourceLists(3)),
    new Puzzle8(sourceLists(3)),
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
    val lefts = result.filter(el => el._2.forall(e => {
      e._1 match {
        case "red" => e._2.toInt <= 12
        case "green" => e._2.toInt <= 13
        case "blue" => e._2.toInt <= 14
        case _ => true
      }
    }))
    println(s"Result of puzzle 3 is: ${lefts.map(s => s._1.toInt).sum}")
  }
}

case class Puzzle4(value: List[String]) extends Puzzle {

  def createMapLine(str: String): Map[String, Map[String, Int]] = {
    val gameNumberParts = str.split(":").map(_.trim).toList
    val gameDayAttempts = gameNumberParts.reverse.head.split(";").toList
    val gameNumber = gameNumberParts.head.split(" ").toList.reverse.head
    val doMap = (for {
      attempt <- gameDayAttempts
      efforts <- attempt.split(", ").toList.map(x => x.trim.split(" "))
    } yield (efforts(0), efforts(1)))
    val dayMap = doMap.groupBy(_._2).map(x => (x._1, x._2.map(y => y._1.toInt).max))
    Map(gameNumber -> dayMap)
  }

  override def run(): Unit = {
    val result = for {
      line <- value
      mapline <- createMapLine(line)
    } yield mapline
    println(s"Result of puzzle 4 is: ${result.map(s => s._2.values.toList.map(_.toInt).product).sum}")
  }
}

case class Puzzle5(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val linesWithIndex = l.zipWithIndex
    val symbolPositions = for {
      line <- linesWithIndex
      ch <- line._1.zipWithIndex
      if !ch._1.isDigit && ch._1 != '.'
    } yield (line._2, ch._2)
    def getNumbersAndPosition(line: String) = {
      for {
        d <- line.zipWithIndex
        if d._1.isDigit
      } yield (d._1, d._2)
    }
    val numbersAndPositions = for {
      line <- l
      np = getNumbersAndPosition(line)
    } yield np

    val consol = for {
      np <- numbersAndPositions
    } yield (np.foldLeft(List[(Int, List[Int])]())((b, a) => a match {
      case (ch, pos) if (b.size > 0 && (b.head._2.head - a._2).abs == 1) => ((b.head._1.toString + ch.toString).toInt, pos :: b.head._2) :: b.tail
      case (ch, pos) if (b.size > 0) => (a._1.toString.toInt, List(a._2)) :: b
      case (ch, pos)  => List((ch.toString.toInt, List(pos)))
    }))

    val inAbove = ((for {
      pos <- symbolPositions
      numbersAbove = consol(pos._1 - 1).filter(l => l._2.toList.contains(pos._2) || l._2.toList.contains(pos._2 - 1) || l._2.toList.contains(pos._2 + 1))
    } yield numbersAbove).flatten).map(s => s._1).sum
    val inBelow = ((for {
      pos <- symbolPositions
      numbersBelow = consol(pos._1 + 1).filter(l => l._2.toList.contains(pos._2) || l._2.toList.contains(pos._2 - 1) || l._2.toList.contains(pos._2 + 1))
    } yield numbersBelow).flatten).map(s => s._1).sum
    val inLeft = ((for {
      pos <- symbolPositions
      numbersLeft = consol(pos._1).filter(l => l._2.toList.contains(pos._2 - 1))
    } yield numbersLeft).flatten).map(s => s._1).sum
    val inRight = ((for {
      pos <- symbolPositions
      numbersRight = consol(pos._1).filter(l => l._2.toList.contains(pos._2 + 1))
    } yield numbersRight).flatten).map(s => s._1).sum

    println(s"Result of puzzle 5 is: ${inAbove + inLeft + inRight + inBelow}")  }
}

case class Puzzle6(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val linesWithIndex = l.zipWithIndex
    val symbolPositions = for {
      line <- linesWithIndex
      ch <- line._1.zipWithIndex
      if ch._1 == '*'
    } yield (line._2, ch._2)

    def getNumbersAndPosition(line: String) = {
      for {
        d <- line.zipWithIndex
        if d._1.isDigit
      } yield (d._1, d._2)
    }

    val numbersAndPositions = for {
      line <- l
      np = getNumbersAndPosition(line)
    } yield np

    val consol = for {
      np <- numbersAndPositions
    } yield (np.foldLeft(List[(Int, List[Int])]())((b, a) => a match {
      case (ch, pos) if (b.size > 0 && (b.head._2.head - a._2).abs == 1) => ((b.head._1.toString + ch.toString).toInt, pos :: b.head._2) :: b.tail
      case (ch, pos) if (b.size > 0) => (a._1.toString.toInt, List(a._2)) :: b
      case (ch, pos) => List((ch.toString.toInt, List(pos)))
    }))

    val allNumbers = for {
      pos <- symbolPositions
      numbersAbove = consol(pos._1 - 1).filter(l => l._2.toList.contains(pos._2) || l._2.toList.contains(pos._2 - 1) || l._2.toList.contains(pos._2 + 1))
      numbersBelow = consol(pos._1 + 1).filter(l => l._2.toList.contains(pos._2) || l._2.toList.contains(pos._2 - 1) || l._2.toList.contains(pos._2 + 1))
      numbersLeft = consol(pos._1).filter(l => l._2.toList.contains(pos._2 - 1))
      numbersRight = consol(pos._1).filter(l => l._2.toList.contains(pos._2 + 1))
      all = numbersAbove ::: numbersBelow ::: numbersLeft ::: numbersRight
      if all.size == 2
    } yield all
     println(s"Result of puzzle 6 is: ${allNumbers.map(s => s(0)._1 * s(1)._1).sum}")
  }

}

case class Puzzle7(l: List[String]) extends Puzzle {
  override def run(): Unit = {
    val list = l.map(s => s.split("\\:")(1)).toList.map(s => s.split("\\|").map(_.trim).toList.map(s => s.split("\\s+").map(_.toInt).toList))
    val result = for {
      card <- list
      myNos = card(0)
      possNos = card(1)
      winningNos = myNos.filter(x => possNos.contains(x))
    } yield winningNos

    println(s"Result of puzzle 7 is: ${result.map(l => if (l.size == 0) 0 else Math.pow(2.0, l.size - 1).toInt).sum}")
  }

}

case class Puzzle8(l: List[String]) extends Puzzle {

  override def run(): Unit = {
    val list: List[List[List[Int]]] = l.map(s => s.split("\\:")(1)).toList.map(s => s.split("\\|").map(_.trim).toList.map(s => s.split("\\s+").map(_.toInt).toList))

//    def accumulate(i: Int): List[(Int, Int)] = {
//      if (i < 0)
//        List((0,0))
//      else {
//        for {
//          cards <- accumulate(i - 1)
//          myNos = list(i)(0)
//          posNos = list(i)(1)
//          winningNos = myNos.filter(s => posNos.contains(s)).size
//          card <- i + 1 to  i + winningNos
//          myCopyNos = list(card)(0)
//          posCopyNos = list(card)(1)
//          copyWinningNos = myCopyNos.filter(s => posCopyNos.contains(s)).size
//        } yield (winningNos, copyWinningNos)
//      }
//    }

//    def accumulate(i: Int): List[Int] = {
//      if (i == 0) List()
//      else
//        for {
//          nos <- accumulate(i - 1)
//          copyNos <- i to i + winningNos(list(i)(0), list(i)(1))
//        } yield copyNos
//    }

    def accumulate(i: Int): Int = {
      def loop(copies: List[List[List[Int]]], accum: Int): Int = copies match {
        case h :: t => {
          val myNos = h(0)
          val posNos = h(1)
          val nos = myNos.filter(s => posNos.contains(s)).size
          loop(t, accum + nos)
        }
        case Nil => accum
      }
      if (i >= list.size) 0
      else {
        (for {
          j <- i to list.size - 1
          myNos = list(j)(0)
          posNos = list(j)(1)
          numCards = myNos.filter(s => posNos.contains(s)).size
          totalNumCards = 1 + loop(list.slice(j + 1, j + numCards + 1), numCards)
        } yield totalNumCards).sum
      }
    }

    val result = accumulate(0)

    println(s"Result of puzzle 8 is: ${result}")
  }
}
