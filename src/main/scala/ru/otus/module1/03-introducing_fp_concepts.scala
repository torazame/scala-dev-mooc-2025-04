package ru.otus.module1

import ru.otus.module1.opt.{Animal, Cat, Dog}

import scala.annotation.tailrec
import scala.language.postfixOps



/**
 * referential transparency
 */




 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = if(n <= 0) 1 else n * factRec(n - 1)


  def factTailRec(n: Int, accum: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if(n <= 0) accum
      else loop(n - 1, n * accum)

    loop(n, 1)
  }





  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   */


}



object hof{

  def dumb(string: String): Unit = {
    Thread.sleep(1000)
    println(string)
  }

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
      val start = System.currentTimeMillis()
      val result = f(a)
      val end = System.currentTimeMillis()
      println(s"Running time: ${end - start}")
      result
  }



  // изменение поведения ф-ции

  def isOdd(i: Int): Boolean = i % 2 > 0
  def not[A](f: A => Boolean): A => Boolean = a => !f(a)
  val isEven: Int => Boolean = not(isOdd)

  isOdd(5) // true
  isEven(5) // false


  // изменение самой функции

  def sum(x: Int, y: Int): Int = x + y

  def partial[A, B, C](a: A)(f: (A, B) => C): B => C =
    f.curried(a)
}


/**
 *  Реализуем тип Option
 */



 object opt {



  trait Animal
  case class Cat() extends Animal
  case class Dog() extends Animal

  /**
   *
   * Реализовать структуру данных Option, который будет указывать на присутствие либо отсутствие результата
   */

  // Invariance
  // + Covariance Если А является подтипом В, то Option[A] является подтипом Option[B]
  // - Contravariance Если А является подтипом В, то Option[A] является супер типом Option[B]

    // Function1[-R, +T]
  val f1: String => Unit = ???
  val f2: Any => Unit = ???

  def foo(f: String => Unit) = f("Hello")

  foo(f2)
  foo(f1)


  sealed trait Option[+T] {
    def isEmpty: Boolean = if(this.isInstanceOf[None.type]) true else false

//    def get: T =  if(this.isInstanceOf[None.type]) throw new Exception("None get")
//      else{
//        val r = this.asInstanceOf[Some[T]]
//        r.v
//      }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
    }

    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit = this match {
      case Some(x) => println(x)
      case None => println("")
    }

    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[B](other: Option[B]): Option[(T, B)] = this match {
      case Some(x) => other match {
        case Some(y) => Some((x, y))
        case None => None
      }
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(predicate: T => Boolean): Option[T] = this match {
      case Some(x) if predicate(x) => Some(x)
      case _ => None
    }
  }

  case class Some[T](v: T) extends Option[T]

  case object None extends Option[Nothing]

  object Option {
    def apply[T](v: T): Option[T] =
      if(v == null) None else Some(v)
  }

  val opt1 : Option[Int] = Option(17)

  val opt2: Option[Option[Int]] = opt1.map(i => Option(i + 1))
  val opt3: Option[Int] = opt1.flatMap(i => Option(i + 1))

  println(opt1)
  println(opt2)
  println(opt3)


 }







 object list {
   /**
    *
    * Реализовать одно связанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содержит первый элемент (голову) и хвост (оставшийся список)
    */

//   def treat(a: Option[Animal]) = ???

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


    case class Cons[T](elem: T, tail: List[T]) extends List[T] {

      def head: T = elem

      def ::[TT >: T](elem: TT): List[TT] = {
        Cons(elem, this)
      }

      def isEmpty: Boolean = {
        head == null & tail == List()
      }

      def :::[TT >: T](other: List[TT]): List[TT] = {
        val reversed = this.reverse
        @tailrec
        def run(lst: List[TT], res: List[TT] = other): List[TT] = {
          if (lst.isEmpty) res
          else run(lst.tail, lst.head :: (res))
        }
        run(reversed)
      }

      def map[TT >: T](f: T => TT): List[TT] = this match {
        case y if y.isEmpty => Nil
        case x: List[T] => f(x.head) :: x.tail.map(f)
      }

      def flatMap[TT >: T](f: T => List[TT]): List[TT] = this match {
        case null => this.asInstanceOf[List[TT]]
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
          lst match {
            case Nil => res
            case _ => run(lst.tail, lst.head :: res)
          }
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
     def apply[A](v: A*): List[A] =
       if(v.isEmpty) Nil
       else Cons(v.head, apply(v.tail :_*))
   }


   def incList(list: List[Int]): List[Int] = {
     list.map(_ + 1)
   }

   def shoutString(list: List[String]): List[String] = {
     list.map(el => s"!$el")
   }


   val l1 = List(1, 2, 3)

   val l2: List[Cat] = List(Cat())



    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например, вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */
    println(l1.reverse.mkString())

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */
    println(l1.map(x => x * x).mkString())

    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */
    println(l1.filter(x => x % 2 == 0).mkString())

    /**
      *
      * Написать функцию incList которая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */
    println(incList(l1).mkString())

    /**
      *
      * Написать функцию shoutString которая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */
    println(shoutString(List("abc", "def", "ghi")))
 }