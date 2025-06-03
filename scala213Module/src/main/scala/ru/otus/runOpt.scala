package ru.otus

import homeworks.opt.{Option, Some, None}

object runOpt extends App {
  println("Примеры использования Option:")
  println("---" * 10)

  println(Some("Some text").map(_.toUpperCase()))
  println(Some("More useless text").flatMap(x => Some(x.toUpperCase())))

  val opt1: Option[Int] = Some(34)
  val opt111: Option[Int] = Some(788)

  val opt2: Option[Int] = opt1.map(i => i + 5)
  val opt3: Option[Int] = opt1.flatMap(i => Option(i + 23))
  val opt4: Option[Int] = opt1.filter(i => i > 50)

  println(s"map: $opt2")
  println(s"flatMap: $opt3")
  println(s"filter $opt4")
  println(s"zip ${opt1.zip(opt111)}")

  print("PrintIfAny: ")
  opt1.printIfAny()

  println(opt1.isEmpty)

}
