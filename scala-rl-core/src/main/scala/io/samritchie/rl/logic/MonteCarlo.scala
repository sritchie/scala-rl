/**
  Logic for playing episodic games.
  */
package io.samritchie.rl
package logic

import cats.{Monad, Monoid}
import cats.implicits._

object MonteCarlo {

  /**
    Aggregating thing that also keeps track of frequencies. The item will be
    paired with a zero if this is the first time seeing it.
    */
  case class FirstVisit[A, B](items: Vector[(A, Int)], frequencies: Map[B, Int], f: A => B) {
    def :+(a: A): FirstVisit[A, B] = {
      val b = f(a)
      val newFrequencies = Util.mergeV(frequencies, b, 1)
      FirstVisit(items :+ ((a, newFrequencies(b) - 1)), newFrequencies, f)
    }
    def iterator: Iterator[(A, Int)] = items.iterator
    def reverseIterator: Iterator[(A, Int)] = items.reverse.iterator
  }
  object FirstVisit {
    def empty[A, B](f: A => B): FirstVisit[A, B] = FirstVisit(Vector.empty, Map.empty[B, Int], f)
    def pure[A, B](a: A, f: A => B): FirstVisit[A, B] = empty(f) :+ a

    def monoid[A, B](f: A => B): Monoid[FirstVisit[A, B]] = new Monoid[FirstVisit[A, B]] {
      val empty: FirstVisit[A, B] = FirstVisit.empty(f)
      def combine(l: FirstVisit[A, B], r: FirstVisit[A, B]): FirstVisit[A, B] =
        FirstVisit(l.items ++ r.items, Monoid[Map[B, Int]].combine(l.frequencies, r.frequencies), l.f)
    }
  }

  /**
    Wrapper around a vector... maybe a vector by itself is enough.
    */
  case class EveryVisit[A](value: Vector[A]) extends AnyVal {
    def :+(a: A): EveryVisit[A] = EveryVisit(value :+ a)
    def iterator: Iterator[A] = value.iterator
    def reverseIterator: Iterator[A] = value.reverse.iterator
  }
  object EveryVisit {
    def empty[A]: EveryVisit[A] = EveryVisit(Vector.empty)
    def pure[A](a: A): EveryVisit[A] = empty :+ a

    implicit def monoid[A]: Monoid[EveryVisit[A]] = new Monoid[EveryVisit[A]] {
      val empty: EveryVisit[A] = EveryVisit.empty
      def combine(l: EveryVisit[A], r: EveryVisit[A]): EveryVisit[A] =
        EveryVisit(l.value ++ r.value)
    }
  }

  /**
    Takes a static policy and a starting state, and a penalty for moves that
    aren't allowed, and returns a context containing the final state and the
    trajectory that got us there.
    */
  def playEpisode[A, Obs, R, M[_], G](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      toG: (Obs, A, R) => G,
      penalty: R
  )(implicit M: Monad[M], G: Monoid[G]): M[(state.This, G)] =
    Monad[M].iterateUntilM((state, G.empty)) {
      case (s, acc) =>
        policy.choose(s).flatMap { a =>
          val nextState = s.act(a).getOrElse((penalty, s).pure[M])
          nextState.map {
            case (r, s2) =>
              (s2, G.combine(acc, toG(s.observation, a, r)))
          }
        }
    }(_._1.isTerminal)

  /**
    Specialized version that keeps track of frequencies too.
    */
  def firstVisit[A, Obs, R, M[_]: Monad](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      penalty: R
  ): M[(state.This, FirstVisit[(Obs, A, R), Obs])] = {
    type FV = FirstVisit[(Obs, A, R), Obs]
    implicit val m: Monoid[FV] = FirstVisit.monoid(_._1)
    playEpisode[A, Obs, R, M, FV](
      policy,
      state,
      (obs, a, r) => FirstVisit.pure((obs, a, r), _._1),
      penalty
    )
  }

  /**
    Version that does NOT keep track of any frequencies.
    */
  def everyVisit[A, Obs, R, M[_]: Monad](
      policy: Policy[A, Obs, R, M, M],
      state: State[A, Obs, R, M],
      penalty: R
  ): M[(state.This, EveryVisit[(Obs, A, R)])] = {
    type EV = EveryVisit[(Obs, A, R)]
    implicit val m: Monoid[EV] = EveryVisit.monoid
    playEpisode[A, Obs, R, M, EV](
      policy,
      state,
      (obs, a, r) => EveryVisit.pure((obs, a, r)),
      penalty
    )
  }
}
