package ru.otus.module1.futures

import ru.otus.module1.futures.HomeworksUtils.TaskSyntax

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличие от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правовой результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */

  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ec: ExecutionContext): Future[(List[A], List[Throwable])] = {
    val empty: Future[(List[A], List[Throwable])] =
      Future.successful(Nil, Nil)

    futures.foldLeft(empty) { (accumulator, future) =>
      accumulator.flatMap {
        case (successes, failures) =>
          future.transform {
            case Success(a) => Success((a +: successes, failures))
            case Failure(e) => Success(successes, e :: failures)
          }
      }
    }.map {
      case (successes, failures) => (successes.reverse, failures.reverse)
    }
  }
}
