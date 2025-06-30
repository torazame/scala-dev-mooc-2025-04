package ru.otus.module3

import zio.{Clock, Scope, Unsafe, ZEnvironment, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.io.StdIn

object ZioApp {

  def main(args: Array[String]): Unit = {

    val environment: ZEnvironment[Clock] =
      ZEnvironment[Clock](Clock.ClockLive)
      val Number = 10000
      // println(zioRecursion.factorial(Number))

      Unsafe.unsafe { implicit u =>
        zio.Runtime.default.unsafe.run(zioConcurrency.printEffectRunningTime(zioConcurrency.r4)
          .provideEnvironment(environment))
      }
  }

}

object ZIOApp2 extends ZIOAppDefault{
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    ZIO.attempt(println("Hello world"))
}