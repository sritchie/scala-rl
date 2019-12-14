/**
  An Agent is a combination of a Policy and a value function.

  The whole markov decision process is a weighted, directed graph. The backup
  diagrams we see are subgraphs;

  The nodes are:

  - State nodes, with edges leading out to each possible action.
  - Action nodes, with edges leading out to (reward, state) pairs.

  Policies are maps of State => Map[A, Weight]. I don't know that I have a
  policy that is NOT that.

  StateValueFn instances are records of the values at particular State nodes.

  So to get the value of an ACTION node you need either:

  - To track it directly, with an ActionValueFn, or
  - to estimate it with some model of the dynamics of the system.

  TODO - Key questions:
  - Can I rethink the interface here? Can StateValueFn instances ONLY be
    calculated for... rings where the weights add up to 1? "Affine combination"
    is the key idea here... a linear combination where the set of scalars adds
    to 1. (Read more about affine combinations here:
    https://www.sciencedirect.com/topics/computer-science/affine-combination)
  - Does that mean that we have a StateValueFn of ONLY DOUBLES?

  # The four key ideas:
  (Policy, EnvModel are a pair)
  (ActionValueFn, StateValueFn are a pair)

  TODO - what is an actual WORLD here? It's something that stochastically
         returns the same things an EnvModel would, of course. An EnvModel
         should give me the dynamics for any particular state I happen to want,
         for anything I happen to find myself in. That can work for Blackjack,
         for example.

  FINAL COMMENTS:
  - Do we have to restrict the type of StateValueFn to be ONLY a double for now,
    until I can generalize?
  - What are the remaining concepts... ways to update these various features by
    walking around in the graph.
  - TODO NEXT - clean up what StateValueFn is actually doing. Can it have an
    internal agg type?
  - AGENT can have an internal thing it uses to track experience. That's the
    object oriented version... and the various algorithms are ways to propagate
    credit back.
  - The more complicated ones can also access the four things that the agent
    has.
  - YOU CAN TOTALLY have a policy that does not need a numeric value... but that
    just needs something with an ordering. So the policy has less stringent
    requirements than the actual estimating thing. TODO can this help me at all?
  - Will I need Value, or ToDouble at the end of all of this?


    Here's a piece on graph learning on reinforcement learning problems:
  http://proceedings.mlr.press/v89/madjiheurem19a/madjiheurem19a.pdf

  A combo of a policy and its value function. Could be helpful navigating the
  monte carlo stuff.

  The bandits are actually agent instances. AND, they probably need to each keep
  their own internal aggregation types to use inside their value functions.

  TODO take these notes.
  - an agent is the only thing that has a full view on the graph.
  - the graph is a directed, weighted graph... to get an expected value you have
    to normalize the weights and multiply each by the values of the next nodes.
  - the "backup diagram" is the subgraph that the agent looked at to make its
    decision.
  */
package io.samritchie.rl

import cats.Monad

trait Agent[Obs, A, @specialized(Int, Long, Float, Double) R, T, M[_]] { self =>
  type This = Agent[Obs, A, R, T, M]

  def monad: Monad[M]
  def policy: Policy[Obs, A, R, M, M]
  def valueFunction: StateValueFn[Obs, T]

  def play(state: State[Obs, A, R, M]): M[(state.This, SARS[Obs, A, R, M])] =
    monad.flatMap(policy.choose(state)) { a =>
      monad.map(state.act(a)) {
        case (r, s2) => (s2, SARS(state, a, r, s2))
      }
    }
}

object Agent {

  /**
    Agent that can't learn.
    */
  case class StaticAgent[Obs, A, R, T, M[_]](
      policy: Policy[Obs, A, R, M, M],
      valueFunction: StateValueFn[Obs, T]
  )(implicit val monad: Monad[M])
      extends Agent[Obs, A, R, T, M]
}
