package ru.otus

import ru.otus.module1.collections.ListLike
import ru.otus.module1.{collections, hof, pattern_matching, type_system}


object App {
  def main(args: Array[String]): Unit = {
    ListLike(1, 2, 3).foreach(println)
    ListLike("a", "b").headOption.foreach(println)

  }
}