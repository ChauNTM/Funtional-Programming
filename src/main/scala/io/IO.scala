package io

import monad.Monad

import scala.io.StdIn.readLine

sealed trait IO[A] { self =>

  def run: A

  def map[B](f: A => B): IO[B] = new IO[B] {
    override def run: B = f(self.run)
  }

  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
    override def run: B = f(self.run).run
  }

}

object IO extends Monad[IO]{
  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)

  def ReadLine: IO[String] = IO { readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temprature in degrees Fahrenheit")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield()

  override def doWhile[A](a: IO[A])(cond: A => IO[Boolean]): IO[Unit] = ???
}
