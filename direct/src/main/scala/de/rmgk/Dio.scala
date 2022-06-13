package de.rmgk

import scala.annotation.compileTimeOnly
import scala.util.Random

class IO[T](val run: () => T):
  def flatMap[B](f: T => IO[B]): IO[B] = new IO(() => f(run()).run())
  def map[B](f: T => B): IO[B]         = IO(f(run()))
  @compileTimeOnly("should have been removed by macro")
  def await: T = ???
object IO:
  def apply[T](run: => T) = new IO(() => run)

@main
def run() =
  val res: IO[Int] = dio {
    //val a = IO(5).await
    //a + 1
    2
  }
  println("runs first")
  println(res.run())
