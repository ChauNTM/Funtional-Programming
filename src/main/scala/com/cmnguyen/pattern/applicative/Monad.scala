package com.cmnguyen.pattern.applicative

import com.cmnguyen.State

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] = flatMap(mf)(f => map(ma)(f))

  override def map[A,B](m: F[A])(f: A => B): F[B] = flatMap(m)(a => unit(f(a)))

  override def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

}

object Monad {

  def eitherMonad[E]: Monad[({ type f[x] = Either[E, x]})#f] = new Monad[({ type f[x] = Either[E, x] })#f] {

    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma flatMap f

  }

  def stateMonad[S]: Monad[({ type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
  }



}
