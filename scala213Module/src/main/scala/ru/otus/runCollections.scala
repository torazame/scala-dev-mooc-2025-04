package ru.otus

import ru.otus.homeworks.collections_1.{calculateFreq, extractBall}

object runCollections extends App {

  // Урна с шарами. 1 = белый шар, 0 = черный шар
  val urn: List[Int] = List(0, 1, 1, 0, 1, 0)

  // Количество испытаний
  val testNum: Int = 10000

  // Коллекция урн
  val urnList: List[List[Int]] = List.fill(testNum)(urn)

  // Проведение множественных испытаний (= размеру коллекции урн с шарами)
  val res: List[Boolean] = urnList.map(extractBall)

  println(s"Наблюдаемая частота появления белого шара = ${calculateFreq(res)}" +
    s".\nТеоретическая вероятность P(A) + P(B) - P(AB) = ${0.6 + 0.5 - 0.6 * 0.5}")
}
