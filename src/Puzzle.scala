import scala.io.Source

trait Puzzle {
  def run(): Unit

}

object Puzzles {
  val sourceLists = (for {
    i <- 1 to 5
  } yield Source.fromFile(s"/Users/martinetherton/Developer/projects/be/scala/aoc2023/src/$i.txt").getLines.toList).toList
  val puzzles = List(
    new Puzzle1(sourceLists(0)),
    new Puzzle2(sourceLists(0)),
    new Puzzle3(sourceLists(1)),
    new Puzzle4(sourceLists(1)),
    new Puzzle5(sourceLists(2)),
    new Puzzle6(sourceLists(2)),
    new Puzzle7(sourceLists(3)),
    new Puzzle8(sourceLists(3)),
    new Puzzle9(sourceLists(4)),
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
    val list = l.map(s => s.split("\\:")(1)).toList.map(s => s.split("\\|").map(_.trim).toList.map(s => s.split("\\s+").map(_.toInt).toList)).toList
    val listWithIndex: List[(List[List[Int]], Int)] = list.zipWithIndex
    def getChildren(l : (List[List[Int]], Int)): List[(List[List[Int]], Int)] = {
      val numberCards = l._1(0).filter(s => l._1(1).contains(s)).size
      listWithIndex.slice(l._2 + 1, l._2 + numberCards + 1)
    }
    def loop(l: List[(List[List[Int]], Int)]): List[(List[List[Int]], Int)] = {
      l match {
        case h :: t => h :: (loop(t) ::: loop(getChildren(h)))
        case _ => Nil
      }
    }

    println(s"Result of puzzle 8 is: ${loop(listWithIndex).size}")

  }
}

case class Puzzle9(lc: List[String]) extends Puzzle {

//  val l1 = List(List(50, 98, 2), List(52, 50, 4))
//  val l2 = l1.foldLeft(Map[Int, Int]())((b, a) => b ++ (for {
//    i <- 0 to a(2) - 1
//    source = i + a(1)
//    destination = i + a(0)
//  } yield (source, destination)).toMap
//  )
//  println(s"map is ${l2}")

  override def run(): Unit = {

    def convertListToMap(l1: List[List[Int]]): Map[Int, Int] = {
      val m = l1.foldLeft(Map[Int, Int]())((b, a) => b ++ (for {
        i <- 0 to a(2) - 1
        source = i + a(1)
        destination = i + a(0)
      } yield (source, destination)).toMap
      )
      m.toMap
    }

    val seedsToFind = lc.head.split(":")(1).trim.split(" ").map(_.toInt).toList
    val l  = lc.tail.zipWithIndex
    val maps = l.collect {
      case ("seed-to-soil map:", i) =>                       ("seed-soil" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      case ("soil-to-fertilizer map:", i) =>           ("soil-fertilizer" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      case ("fertilizer-to-water map:", i) =>         ("fertilizer-water" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      case ("water-to-light map:", i) =>                   ("water-light" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      case ("light-to-temperature map:", i) =>       ("light-temperature" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      case ("temperature-to-humidity map:", i) => ("temperature-humidity" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      case ("humidity-to-location map:", i) =>       ("humidity-location" -> convertListToMap(l.slice(i + 1, l.size).takeWhile(_._1 != "").map(_._1).map(s => s.split(" ").toList.map(_.toInt))))
      //case _ => List()
    }.to(Map)
    //}.filter(_ != List())
    val locations = seedsToFind.foldLeft(List[Int]())((b, a) => {
      val i = maps("seed-soil").getOrElse(a, a)
      println(s"value: ${maps("seed-soil").getOrElse(a, a)}")
      val j = maps("soil-fertilizer").getOrElse(i, i)
      val k = maps("fertilizer-water").getOrElse(j, j)
      val l = maps("water-light").getOrElse(k, k)
      val m = maps("light-temperature").getOrElse(l, l)
      val n = maps("temperature-humidity").getOrElse(m, m)
      val o = maps("humidity-location").getOrElse(n, n)
      o :: b
    })
    println(s"Result of puzzle 9 is: ${locations.min}")
  }
}