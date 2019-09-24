package io.samritchie.rl

import cats.{Monad, StackSafeMonad}

/**
  Identity type with its own Monad.
  */
case class Id[+A](value: A) extends AnyVal

object Id {
  implicit val idMonad: Monad[Id] = new StackSafeMonad[Id] {
    def pure[A](a: A): Id[A] = Id(a)
    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a.value)
  }
}
