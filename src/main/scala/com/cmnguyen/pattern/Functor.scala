package com.cmnguyen.pattern

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}
