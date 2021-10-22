package com.cmnguyen.pattern.applicative

import com.cmnguyen.State
import com.cmnguyen.State._
import com.cmnguyen.pattern.applicative.Applicative._
import com.cmnguyen.pattern.monad.Functor
import com.cmnguyen.pattern.monoid.{Foldable, Monoid}

trait Traverse[F[_]] extends Functor[F] with Foldable[F]{ self =>
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = a
  }
  override def map[A, B](a: F[A])(f: A => B): F[B] = traverse[Id, A, B](a)(f)(idMonad)

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({ type f[x] = Const[B, x] })#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  /**
   * this function equivalent with
   * def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
   * traverse(ta)(a => for {
   *   i <- State.get[Int]
   *   _ <- State.set(i + 1)
   * } yield (a, i))(Monad.stateMonad).run(0)._1
   * */
  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta)(a => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)(a => for {
      as <- get[List[A]]
      _ <- set(a::as)
    } yield ()).run(List[A]())._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(a => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  def toList1[A](fa: F[A]): List[A] = mapAccum(fa, List[A]())((a, s) => ((), a::s))._2.reverse

  def zipWithIndex1[A](ta: F[A]): F[(A, Int)] = mapAccum(ta, 0)((a, s) => ((a, s), s+1))._1

  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z)((a, s) => ((), f(s, a)))._2

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb))({
      case (a, Nil) => ((a, None), Nil)
      case (a, h::t) => ((a, Some(h)), t)
    })._1

  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa))({
      case (b, Nil) => ((None, b), Nil)
      case (b, h::t) => ((Some(h), b), t)
    })._1

  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse(fa)(f)(G) -> traverse(fa)(g)(H)

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({ type f[x] = F[G[x]]})#f] =
    new Traverse[({ type f[x] = F[G[x]]})#f] {
      override def traverse[G[_] : Applicative, A, B](fa: F[G[A]])(f: A => G[B]): G[F[G[B]]] =
        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    }

  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({ type f[x] = F[G[x]]})#f] =
    new Monad[({ type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        F.flatMap(ma)(ga => F.map(T.traverse(ga)(f)(F))(G.join))
    }
}

case class Tree[+A](head: A, tail: List[Tree[A]]) {
  def map[B](f: A => B): Tree[B] =
    Tree(f(head),
      tail.foldLeft(List[Tree[B]]())((b, a) => b :+ a.map(f)))
}

object Traverse {
  val listTraverse: Traverse[List] = new Traverse[List] {
    override def map[A, B](a: List[A])(f: A => B): List[B] = a map f

    override def sequence[G[_], A](fga: List[G[A]])(implicit G: Applicative[G]): G[List[A]] = {
      fga.foldLeft(G.unit(List[A]()))((b, ga) => G.map2(b, ga)(_ :+ _))
    }
  }

  val optionTraverse: Traverse[Option] = new Traverse[Option] {
    override def map[A, B](a: Option[A])(f: A => B): Option[B] = a map f

    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case None => G.unit(None)
        case Some(a) => G.map(f(a))(b => Some(b))
      }

    override def sequence[G[_] : Applicative, A](fga: Option[G[A]]): G[Option[A]] = traverse(fga)(ga => ga)
  }

  val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](a: Tree[A])(f: A => B): Tree[B] = a map f

    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}
