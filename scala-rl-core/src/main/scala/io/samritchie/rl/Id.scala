package io.samritchie.rl

import cats.{Bimonad, Comonad, Monad, StackSafeMonad}

/**
  Identity type with its own Monad.
  */
case class Id[+A](value: A)

object Id {
  implicit val idMonad: Monad[Id] = IdMonad
  implicit val idComonad: Comonad[Id] = IdMonad

  object IdMonad extends StackSafeMonad[Id] with Bimonad[Id] {
    def pure[A](a: A): Id[A] = Id(a)
    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a.value)
    override def coflatMap[A, B](id: Id[A])(f: Id[A] => B): Id[B] =
      Id(f(id))
    override def extract[A](id: Id[A]): A = id.value
  }
}
