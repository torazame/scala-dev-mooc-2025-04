package ru.otus.homeworks

import scala.annotation.tailrec

/**
 *
 * Реализовать одно связанный иммутабельный список List
 * Список имеет два случая:
 * Nil - пустой список
 * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
 */

/**
 * Для этой структуры данных реализуйте набор методов:
 *
 * ::
 * apply
 * mkString
 * reverse
 * map
 * flatMap
 * filter
 * incList
 * shoutString
 */

object list {

  sealed trait List[+T] {
    def head: T
    def tail: List[T]
    def isEmpty: Boolean
    def ::[TT >: T](elem: TT): List[TT]
    def :::[TT >: T](list: List[TT]): List[TT]
    def map[TT >: T](f: T => TT): List[TT]
    def flatMap[TT >: T](f: T => List[TT]): List[TT]
    def filter(predicate: T => Boolean): List[T]
    def reverse: List[T]
    def mkString(sep: String = ", "): String
  }

  case class Cons[T](head: T, tail: List[T]) extends List[T] {

    def isEmpty: Boolean = {
      head == null & tail == List()
    }

    def ::[TT >: T](elem: TT): List[TT] = {
      Cons(elem, this)
    }

    def :::[TT >: T](other: List[TT]): List[TT] = {
      val reversed = this.reverse
      @tailrec
      def run(lst: List[TT], res: List[TT] = other): List[TT] = {
        if (lst.isEmpty) res
        else run(lst.tail, lst.head :: res)
      }
      run(reversed)
    }

    def map[TT >: T](f: T => TT): List[TT] = this match {
      case y if y.isEmpty => Nil
      case x: List[T] => f(x.head) :: x.tail.map(f)
    }

    def flatMap[TT >: T](f: T => List[TT]): List[TT] = this match {
      case y if y.isEmpty => Nil.asInstanceOf[List[TT]]
      case x: List[TT] => f(x.head).:::(x.tail.flatMap(f))
    }

    def filter(predicate: T => Boolean): List[T] = {
      @tailrec
      def run(lst: List[T], res: List[T] = List()): List[T] = {
        lst match {
          case Nil => res
          case _ => if (predicate(lst.head)) {
            run(lst.tail, lst.head :: res)
          } else {
            run(lst.tail, res)
          }
        }
      }
      run(this.asInstanceOf[List[T]])
    }

    def reverse: List[T] = {
      @tailrec
      def run(lst: List[T], res: List[T] = List()): List[T] = {
        if (lst.isEmpty) res
        else run(lst.tail, lst.head :: res)
      }
      run(this.asInstanceOf[List[T]])
    }

    def mkString(sep: String = ", "): String = {
      @tailrec
      def run(lst: List[T], res: String = ""): String = {
        lst match {
          case Nil => s"List($res)"
          case _ => if (res.isEmpty) {
            run(lst.tail, res + s"${lst.head}")
          } else {
            run(lst.tail, res + sep + s"${lst.head}")
          }
        }
      }
      run(this.asInstanceOf[List[T]])
    }
  }

  case object Nil extends List[Nothing] {
    override def head: Nothing = throw new NoSuchElementException("No element")
    override def tail: List[Nothing] = throw new NotImplementedError("No elements inside")
    override def isEmpty: Boolean = true
    override def ::[T](element: T): List[T] = List(element)
    override def :::[T](other: List[T]): List[T] = other
    override def map[T](f: Nothing => T): List[T] = Nil
    override def flatMap[T](f: Nothing => List[T]): List[T] = Nil
    override def filter(predicate: Nothing => Boolean): List[Nothing] = Nil
    override def reverse: List[Nothing] = Nil
    override def mkString(sep: String): String = ""
  }

  object List {
    def apply[T](v: T*): List[T] = {
      if (v.isEmpty) Nil
      else Cons(v.head, apply(v.tail: _*))
    }
  }

}
