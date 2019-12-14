# The Plan

What am I going to need to make?

- various games and worlds
- MDP code with proper interfaces
- 10-arm test harness...
- some interface for the world to interact
- various programming demos, like the racetrack thing

Do I need to bring some probabilistic programming ideas into the fold here? Maybe not at first, maybe that's too turbo?

Each of these needs to have its own visualization.

Can I go totally overboard and integrate this with a website where you can interact? Where you can actually have some way of playing the games, or setting the parameters?

## Probabilistic Programming

Does this help me at all? https://github.com/stripe/rainier/blob/develop/docs/tour.md

## TODO

Things I felt at one point were important..

  - make DecayState work with a RING, not with anything so generic! And
    specialize it. (investigate what this means.)
  - we want the aggregator that currently deals with Value instances to take a
    Double only in the case with gamma = 1.0, a Left(instance) in the case where
    gamma = 0.0, and some generic thing...
  - rewrite ActionValueMap in terms of a default and a base.
