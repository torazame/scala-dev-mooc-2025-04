package ru.otus

import homeworks.list.{List, Nil}

object runList extends App {

  /**
   *
   * Написать функцию incList которая будет принимать список Int и возвращать список,
   * где каждый элемент будет увеличен на 1
   */
  def incList(list: List[Int]): List[Int] = {
    list.map(_ + 1)
  }

  /**
   *
   * Написать функцию shoutString которая будет принимать список String и возвращать список,
   * где к каждому элементу будет добавлен префикс в виде '!'
   */
  def shoutString(list: List[String]): List[String] = {
    list.map(el => s"!$el")
  }

  // Примеры

  val list1: List[Int] = List(2, 3, 5, 4)
  val list2 = 23 :: list1
  val list3: List[List[Int]] = List(List(1, 2), List(4, 6), List(5, 7, 9))
  val stringList = List("abc", "def", "ghi")

  println(s"List1 is ${list1.mkString()}")
  println(s"List2 is ${list2.mkString()}")
  println(s"List3 is $list3")
  println(s"StringList is $stringList")
  println("---" * 20)

  println(s"Squared: ${list1.map(x => x * x).mkString()}")

  println(s"Flatmapped: ${list3.flatMap {
    x => x.map(y => y * y * y)
  }.mkString() }")

  println(s"Concatenated list: ${list1.:::(list2).mkString()}")
  println(s"Concatenated with Nil: ${(list1 ::: List()).mkString()}")
  println(s"Reversed list1: ${list1.reverse.mkString(" * ")}")
  println(s"Filtered list1: ${list1.filter { x: Int => x % 2 == 0}.mkString()}")

  println(list1.mkString("\n"))
  println(list1.mkString(" | "))
  println("---" * 3)
  println("Применение функций:\n")
  println(s"Incremented list: ${incList(list1).mkString()}")

  println(s"Shout list: ${shoutString(stringList).mkString()}")
  println("---" * 3)

  println("Работа с Empty:\n")
  println(s"Empty flatMapped: ${List().flatMap {x: Int => List(x, x * 2)}}")
  println(s"Empty mapped: ${List().map((x: Int) => x * 5)}")

  println(s"Empty concatenated: ${List() ::: List(2, 7, 3)}")

}
