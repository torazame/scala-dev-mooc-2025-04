package ru.otus

import ru.otus.module1.type_system


object App {
  def main(args: Array[String]): Unit = {

    println(type_system.v1.foo())

  }
}