---
layout: docs
title:  "Planning and Learning with Tabular Methods"
section: "course"
---

# Planning and Learning with Tabular Methods

{% include_relative warning.md %}

## Notes

iirc this is more of a summary of everything that came before, yes, uniting it into the big approach.

## Chapter Sketch

There is a ton in here. Does this unify, or should I chill out and go back to the good stuff? Try to get a feel for what this is introducing... then I can start coding.

We start with tabular dyna-Q, then slowly make it more complicated.

## Models and Planning

This introduces the idea that models let you plan but updating your state-value or action-value functions based on predictions... the map is not the territory, but you can increase your understanding of the map.

We are now back to the idea of a model, vs an actual environment to interact with. How interesting, similar to consciousness, here... we are always interacting with the model, and then some separate process behind the scenes is updating the model to be more like the real world.

* distribution models
* sample models

Remember the two?

Can you just draw a reward/state pair? Or do you get the entire distribution to look at? That's the difference.

Planning - take either a model or (model, policy) and produces an improved policy.

* there is also a thing called a PLAN SPACE MODEL, which we don't consider further, but this seems super cool. You have functions from plans to plans??

demo - random-sample one-step tabular Q-planning.

planning in very small steps, too. They love online.

### Dyna: Integrated Planning, Acting, and Learning

Dyna-Q.

There are two functions for real-world experience:

* improve the model
* directly improve the value function.

What is tabular Dyna-Q? (obviously this is an interface, and we should be able to plug in various versions!)

* planning method? one-step tabular q-planning
* direct RL? one-step tabular Q-learning
* The MODEL is super dumb and just assumes a deterministic transition - whatever happened last time will happen again, for (s, a) => (r, s)

Okay, I'm going to need to implement this to explain it more clearly. Go back here and get the interface going.

## When the Model is Wrong

The model above can be wrong with stochastic info, or with very little info to build a stochastic model.

If you have a fucked up model usually you will run into a clash with reality, and that will correct the model.

A worse thing is when the environment becomes BETTER than it was before, and we don't get to adjust in time.

Well, we can give a reward bonus. That is a nice way to deal. And implement the measurement i guess that shows how it gets better?

## Prioritized Sweeping

okay, before we were doing a sweep that just randomly selected states from what we saw before.

We really want to work backward from any state whose value has just changed. That is where we can ripple the updates from.

Does that imply that we want a distance metric? That would help, right?

A function to get the set of states that are blah steps away? A graph model really would be nice in this case.

algorithm on page 170... can we do this better?

"sample updates", page 171.

## Expected vs Sample Updates

I guess we get here into what sample updates actually are.

Oh, this is at the end... do we want to just sample at the VERY end, or do we use the expectation give the model?

What if we sample within quantiles or something? We don't weight everything? That is, again, a way to get a sample model...

Do we need to do any testing here?

Maybe just make sure that our code harness COULD test this stuff.

## Trajectory Sampling

Sample according to the current policy, within the model! This is called trajectory sampling.

vast, boring parts of the state-space are ignored.

## Real-time Dynamic Programming

Another algo that we can talk about, complicated, get into it if we decide to show this off. Page 178 shows the goodies.

## Planning at Decision Time

We've been talking about background planning; you can give a budget here and plan away in the background.

Woah... what if we use planning, based on the model, to roll forward? Use the CURRENT STATE in the actual game, like chess or something, to decide what move to do?
Do you plan at decision time, when given a state? Or do you plan ahead of time, trying to guess what states you'll be in, and then rapidly get after it?

## Heuristic Search

Classic decision time algos!

Implement this shit, and save the results.

## Rollout Algorithms

> Rollout algorithms are decision-time planning algorithms based on Monte Carlo control applied to simulated trajectories that all begin at the current environment state. (p. 183)

They do a ton of monte-carlo guesses and then use that info to decide whether or not to store the value functions.

This comes from Backgammon, where you "roll out" the dice a bunch of times to see what might happen next.

There is a rollout policy...

Figure out how this works, implement once I have my existing goodies.

You want to save info?

## Monte Carlo Tree Search

Well, you should do monte carlo tree search.

This is an amazing one that I definitely can't wait to implement! And now we are into some serious modern shit.

## Summary of Part 1

Woah, what a haul.

Lots of different dimensions... p.191 covers these. There is so much to cover in this fucking reinforcement learning writeup! I am going to be such a stud if I can tackle all of this. I just need to be coding and writing all the time, now... And getting my neural network stuff down.

## Programming Exercises

* function to go from a distribution model to a sample model. (these should be graphs so we can do breadth first searches out from areas that have changed.)
* interfaces for sample and distribution models?
* q-planning, page 161
* dyna-Q
* dyna-Q+, with a bonus that encourages exploration.
* have the bonus implemented in different spots... just like in ex 8.4
* trajectory sampling.
* replicate figure 8,8...I have the code, do it in scala, change b=3.
* RTDP on the racetrack, show that off!
* heuristic search.
