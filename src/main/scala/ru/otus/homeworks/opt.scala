package ru.otus.homeworks

/**
 *
 * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
 */

object opt {

  sealed trait Option[+T] {
    def isEmpty: Boolean = if (this.isInstanceOf[None.type]) true else false

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case None => None
      case Some(v) => f(v)
    }

    def printIfAny(): Unit = this match {
      case Some(x) => println(x)
      case None => println("")
    }

    def zip[B](other: Option[B]): Option[(T, B)] = this match {
      case Some(x) => other match {
        case Some(y) => Some((x, y))
        case None => None
      }
    }

    def filter(predicate: T => Boolean): Option[T] = this match {
      case Some(x) if predicate(x) => Some(x)
      case _ => None
    }
  }

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  object Option {
    def apply[T](v: T): Option[T] =
      if (v == null) None else Some(v)
  }

}
