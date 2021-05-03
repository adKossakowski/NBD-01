package pl.edu.pja

import scala.annotation.tailrec
import scala.math.abs

object Main {

  def main(args: Array[String]): Unit = {
    val weekDays = List("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")
    val productMap = Map("Pomarańcze" -> 4.40, "Banany" -> 2.24, "Gruszki" -> 1.25, "Orzeszki" -> 10.2, "Sałata" -> 7.80)
    val intList = List(0, 1, 22, 0, 3, 40, 0, 5, 6, 0, 7, 8, 9, 0, 10, 11)
    val doubleList = List(1.22, -10.0, -2.1, -1.45, 2.6, 6.5, 12.6)

    println(zadanie1a(weekDays))
    println(zadanie1b(weekDays))
    println(zadanie1c(weekDays))

    println("Zadanie 2 podpunkt a:\n " + zadanie2A(weekDays))
    println("Zadanie 2 podpunkt b:\n " + zadanie2A(weekDays.reverse))

    println("Zadanie 3: " + tailRecursiveString(weekDays, ""))


    println("Zadanie 4a: " + foldLeftFunction(weekDays))
    println("Zadanie 4b: " + foldRightFunction(weekDays))
    println("Zadanie 4c: " + foldLeftFilterFunction(weekDays, "P"))

    println("Zadanie 5: " + zadanie5(productMap, 10))

    zadanie6Krotka(Tuple3("Nazwa", 1, 1.33))

    zadanie7Option(productMap, "Banany")
    zadanie7Option(productMap, "Rukola")
    println("Zadanie 8 - " + zadanie8FilterRec(0, intList, List()))
    println("Zadanie 9 - " + zadanie9Increase(1, intList))
    println("Zadanie 10 - " + zadanie10Fun(-5, 12, doubleList))
  }

  def zadanie1a(weekDays: List[String]): String = {
    println("Zadanie 1 - podpunkt a")
    var firstString: String = ""
    for (i <- weekDays.indices) {
      if (firstString.nonEmpty)
        firstString += ", "
      firstString += weekDays(i)
    }
    firstString
  }

  def zadanie1b(weekDays: List[String]): String = {
    println("Zadanie 1 - podpunkt b")
    var secondString: String = ""
    for (i <- weekDays.indices) {
      if (weekDays(i).startsWith("P")) {
        if (secondString.nonEmpty)
          secondString += ", "
        secondString += weekDays(i)
      }
    }
    secondString
  }

  def zadanie1c(weekDays: List[String]): String = {
    println("Zadanie 1 - podpunkt c")
    var thirdString: String = ""
    var elementIndex = 0
    while (elementIndex < weekDays.length) {
      if (thirdString.nonEmpty)
        thirdString += ", "
      thirdString += weekDays(elementIndex)
      elementIndex += 1
    }
    thirdString
  }

  def zadanie2A(weekDays: List[String]): String = {
    var retStr = ""
    if (weekDays.nonEmpty) {
      if (weekDays.tail.nonEmpty)
        retStr += weekDays.head + ", " + zadanie2A(weekDays.tail)
      else
        retStr += weekDays.head + zadanie2A(weekDays.tail)
    }
    retStr
  }

  @tailrec
  def tailRecursiveString(list: List[String], accumulator: String): String = list match {
    case Nil => accumulator
    case head :: tail =>
      var addStr = ""
      if (accumulator.nonEmpty)
        addStr += ", "
      addStr += head
      tailRecursiveString(tail, accumulator + addStr)
  }

  def foldLeftFunction(weekDays: List[String]): String = {
    weekDays.tail.foldLeft(weekDays.head)(_ + ", " + _)
  }

  def foldLeftFilterFunction(weekDays: List[String], filter: String): String = {
    val tmpList = weekDays.filter(_.startsWith(filter))
    tmpList.tail.foldLeft(tmpList.head)(_ + ", " + _)
  }


  def foldRightFunction(weekDays: List[String]): String = {
    weekDays.foldRight("") {
      (a, b) => {
        if (b.nonEmpty)
          a + ", " + b
        else a
      }
    }
  }

  def zadanie5(productMap: Map[String, Double], percentage: Int): Map[String, Double] = {
    productMap.map(e => e._1 -> (e._2 + (e._2 * percentage / 100)))
  }

  def zadanie6Krotka(tuple: (String, Int, Double)): Unit = {
    println("Wartość pierwsza krotki: " + tuple._1)
    println("Wartość druga krotki: " + tuple._2)
    println("Wartość trzecia krotki: " + tuple._3)
  }

  def zadanie7Option(productMap: Map[String, Double], szukajProdukt: String): Unit = {
    println("zadanie7Option - cena produktu: " + szukajProdukt + " -> " + productMap.getOrElse(szukajProdukt, "Brak produktu w ofercie"))
  }


  @tailrec
  def zadanie8FilterRec(n: Int, l: List[Int], acc: List[Int]): List[Int] = l match {
    case Nil => acc.reverse
    case head :: tail if head == n => zadanie8FilterRec(n, tail, acc)
    case head :: tail => zadanie8FilterRec(n, tail, head :: acc)
  }


  def zadanie9Increase(i: Int, intList: List[Int]): List[Int] = {
    intList.map(e => e + i)
  }

  def zadanie10Fun(min: Double, max: Double, dList: List[Double]): List[Double] = {
    dList.filter(e => min <= e && e <= max).map(abs)
  }


}
