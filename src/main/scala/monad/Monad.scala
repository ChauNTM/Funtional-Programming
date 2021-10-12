package monad

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldLeft(unit(List[A]()))((fla, fa) => flatMap(fa)(a => map(fla)(a::_)))

  //  def sequence[A](lma: List[F[A]]): F[List[A]] =
  //    lma.foldRight(unit(List[A]()))((fa, fla) => map2(fa, fla)(_::_))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_::_))

  //  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def compose[A, B, C](fa: A => F[B], fb: B => F[C]): A => F[C] = a => flatMap(fa(a))(fb)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(fa => fa)

}

object Monad {

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f

    override def doWhile[A](a: Stream[A])(cond: A => Stream[Boolean]): Stream[Unit] = for {
      a1 <- a
      ok <- cond(a1)
      _ <- if (ok) doWhile(a)(cond) else unit(())
    } yield ()
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f

    override def doWhile[A](a: List[A])(cond: A => List[Boolean]): List[Unit] = for {
      a1 <- a
      ok <- cond(a1)
      _ <- if (ok) doWhile(a)(cond) else unit(())
    } yield ()
  }

}
