package ru.otus


object App {
  def main(args: Array[String]): Unit = {

    val two = (x: Int) => {
      return x
      2
    }

    def sumItUp: Int = {
      def one(x: Int): Int = {
        return x
        5
      }

      1 + one(2) + two(5) // 1 + 2 + 3
    }

    println(sumItUp)

  }
}