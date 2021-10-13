package com.cmnguyen.pattern

import com.cmnguyen.State

trait Monad[F[_]] {

  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]


  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((fa, b) => map2(fa, b)(_::_))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, b) => map2(f(a), b)(_::_))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def compose[A, B, C](fa: A => F[B], fb: B => F[C]): A => F[C] = a => flatMap(fa(a))(fb)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

}

// Id monad
case class Id[A](value: A) {

  def map[B](f: A => B): Id[B] = Id(f(value))

  def flatMap[B](f: A => Id[B]): Id[B] = f(value)

}

object Monad {

  val streamMonad: applicative.Monad[LazyList] = new applicative.Monad[LazyList] {
    override def unit[A](a: => A): LazyList[A] = LazyList(a)

    override def flatMap[A, B](ma: LazyList[A])(f: A => LazyList[B]): LazyList[B] = ma flatMap f
  }

  val listMonad: applicative.Monad[List] = new applicative.Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  def getState[S]: State[S, S] = State(s => (s, s))
  def setState[S](s: => S): State[S, Unit] = State(_ => ((), s))

  def stateMonad[S]: applicative.Monad[({ type f[x] = State[S, x] })#f] = {
    new applicative.Monad[({type f[x] = State[S, x]})#f] {

      override def unit[A](a: => A): State[S, A] = State(s => (a, s))

      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma.flatMap(f)

    }

  }

  /**
   * Example for applying StateMonad
   */
  val F = stateMonad[Int]
  def zipWithIndex[A] (as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- getState
      _ <- setState(n + 1)
    } yield (n -> a) :: xs).run(0)._1

}
