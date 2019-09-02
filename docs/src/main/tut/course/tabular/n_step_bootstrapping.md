---
layout: docs
title:  "n-step Bootstrapping"
section: "course"
---

# n-step Bootstrapping

{% include_relative warning.md %}

# Notes

Really, this is the main thing we should be coding. And then we can specialize back to get everything else. Build up, as in the book, but I need to know where I'm building toward!

These let us shift smoothly between the TD(0) method and the full-on, infinite step Monte Carlo methods. We hear another mention of eligibility traces... but I don't know what that is, so let's move on!

n-step methods allow bootstrapping to occur very fast, to get online behavior, but also if we have time to start extending updates out into the future, to get the benefit of the long view.

* Prediction first,
* then control.

We start with n-step Sarsa... but then we come up with some other methods that are in fact extensions of a grand unifying thing! That I want to go into.

We sort of have to solve for THIS stuff... and then all of the previous work becomes a generalization. Well, sort of. Dynamic programming becomes a... is it a special case, still? I guess not since we don't have the dynamics at play. But if we do then we can use them. That is what comes back in chapter 8.

## Chapter Sketch

The individual chapters. What is up?

### n-step TD Prediction

Nice picture of the backup diagrams, which I still don't really get / like. What do we do if we want to extend beyond that first step? We need to keep playing a game, generating an episode, but as we play we can start passing info back.

> For example, a two-step reward would be based on the first two rewards and the estimated value of the state two steps later. (p. 142)

> The methods are still TD methods because they still change an earlier estimate based on how it differs from a later estimate. (p. 142)

This still is a constant-alpha method; *BUT NOTE* that we are sneaking in an idea with the constant alpha thing!!! That is definitely NOT the only way to aggregate this stuff! That is just a way to privilege recent games. You can totally have different methods of aggregation.

Then we look at the random walk task about show how different settings of alpha or n cause different overshoots. Thing to note is that an intermediate value of n worked nicely, halfway between monte carlo and TD.

This is a state-value updater because the policy already decides what to do... it has some action, presumably, that it is going to take for everything. Or some chance of taking each action that it based on an action-value function that you assume someone else gave you. It definitely has knowledge of the actions, in any case.

### n-step Sarsa

Remember, to come up with a policy we need to switch from state-value to action-value. That way we can use the collection of action-values to modify the policy, potentially.
We still need the guarantee that we're possibly going to explore all states, so for on policy we need an $\epsilon\text{-greedy}$ method.

Keep the action-value around then update.

Expected sarsa is easy... just use the expected approximate values at the end.

### n-step Off-Policy Learning

Woah, generalize again! Obviously I have to code this first, and then specialize it back to all of the previous stuff. Show how single state gives you bandits, for example.

Note that our importance sampling ratio starts and ends one step later than if we were estimating the action-value.

off-policy n-step expected sarsa too, here. just showing off.

### Per-decision methods with Control Variates

Research topic alert! Look into this more in the exercises, but this should just be a different aggregator. This is a way of saying, okay, my update is actually a weighted average of the training policy's thing, what it learned, and then what the target policy ALREADY has.

This is using importance sampling in place of alpha, I think? Think of a way to describe what is happening. But show again that it is just another aggregator.

### Off policy learning WITHOUT importance sampling - the n-step tree backup algo

Is there a non-importance-sampling method that we can use for off-policy learning? We did this for the one-step case before... can we do this for the good stuff, the multi-step?

You can get some budget and go chase promising stuff, unfolding as you go, if you have time.

You're sort of learning from your model. I like it, easy.

### n-step $Q(\sigma)$

Can we unify all of the algos we've already seen?

yeah, this is what to actually code, since we can... show that the $\sigma$ parameter can be set anywhere from 0 to 1.

## Programming Exercises

* Note that when we do the constant alpha thing, that is just a particular way to aggregate. If we used $1 / n$ then we'd be doing a straight-up average. But you can weight different games differently, even, instead of going by time. The reason to weight games played more recently is that you've got more experience that went into choosing that trajectory.
* Maybe when you average in the monoid you do a different thing - you weight by how many games you've played, instead of by alpha.
* exercise 7.2... plug in the "sum of TD errors" thing from 7.1! That is actually a DIFFERENT way of propagating the information around. Implement this as an aggregator as well.
* implement n-step sarsa
* expected n-step sarsa
* q-learning
* Can we do a sliding window product? I think so right? Since products are just repeated addition? Well, that's a nice extension.
* exercise 7.10, implement the control variates thing.
* off-policy without importance sampling... tree backup algo!
* n-step q-sigma
