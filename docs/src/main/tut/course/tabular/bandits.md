---
layout: docs
title:  "Multi-armed Bandits"
section: "course"
---

# Multi-Armed Bandits

Halmos demonstrates some extensions of the concept by introducing the ideas of relations that are:
- reflexive, where $x R x$, or $(x, x) \in R$, for every $x$ in $X$
- symmetric, where $x R y$ implies $y R z$
- transitive, where $x R y$ and $y R z$ imply $x R z$



This comes from chapter 2 of the RL book, and is part of my series on reinforcement learning.

This chapter introduces the kind of thing we're going to be dealing with. What is my unique contribution here? The problem framework, its types, are interesting, and the way you play. By abstracting out the structure, you can go to town on the various pieces that we plug in to the game. Much of the innovation presented in the RL book is various tweaks on the plugins.

## Discussion

Remember, for each of these, the goal should be to come up with a one paragraph description of what the chapter is about.

Example of a style of method that does NOT contextualize... but has some relationships to what comes later.

how do we learn anything at all when there's a single state, let alone multiple? Turns out this is an area called "multi-armed bandits". This is an overview that we maybe used at Twitter to deploy models. If you can get realtime feedback (like with an ads system!) you can use a system like this.

A key point here is evaluative vs instructive feedback.

This is not meant to be a total exploration of the bandit problem in Scala. They're just trying to use this to introduce the later ideas... so I need to use this chapter to introduce simple versions of the interfaces that will later reign. That is going to be my contribution here.

What's the objective here? You get these levers, you do something, you get a reward. How do you act so as to maximize the expected reward?

Already the assumptions start flying, as, of course, they must. You're making the assumption that there is some logic to the rewards! That it's not totally random. Does this make sense? Well... how else can you act, I guess?

Now we start introducing some terminology that will come in later - the $q^*(a)$ function, the action value. There is no state yet.

There is the problem, always here, of balancing exploration vs exploitation.

### Action Value

Sample average... then select the best. Seems reasonable!

Already we have a place where we can start to put in different functions. What about the latest? This is already a simplified version of the more difficult thing, when you've got a state.

This section introduces how to think about what exactly you're trying to maximize. Implicit is that you're keeping a table around, and updating values in it, then inspecting some range in that table.

This give you the ability to choose exploit / explore.

### 10-armed Testbed
https://datascience.stackexchange.com/questions/15415/what-is-the-reward-function-in-the-10-armed-test-bed

A demo of the 10 armed testbed in python here: https://github.com/SahanaRamnath/MultiArmedBandit_RL

### Incremental Implementation
This is the key to online learning. How can we write an interface to cover what's happening here?

Kind of lame to cover this... link to Algebird. We want an aggregator that can absorb new rewards into the old estimate. Turns out there is a more general idea at play here.

### Non-stationary problem

What is interesting about this? Well, we want to start ignoring old information.
p. 33 talks about convergence guarantees, and how we don't actually want to converge if we want to be open to new information.

Exponential recency-weighted average is a nice way around this. This is a way of swapping in a different implementation of the aggregator!

Another way would be sliding windows. Can we go into that? Why would you want that? You... wouldn't, as much. But the door is open now to different ways of exploring.

### Optimistic Initial Values

This strikes me as a goofy way to solve the problem, but a nice way to get the system to explore the space, if you're going to stick within the bounds supplied. How do you FORCE the system to choose all options, to at least explore everything once? One way is just to make everything irresistable.

### UCB Action Selection

This is a way of prioritizing states that haven't been explored in a while. Can I get this in to my interface? I bet you could add a term that's calculated based on how many times we've actually seen something... like, you track a count of times you've chosen vs rewards or something. Oh, you can. It's just the denominator. So I think you have everything here to implement this stuff behind various interfaces.

Then picking what you want becomes... a hyperparameter optimization. Or, at least, you can investigate how these various systems work.

### Gradient Bandit Algorithms

I don't know if I need to go into this one, as it doesn't come up much later. No need to cover absolutely everything in the book. But give it a look, see if it fits within the model.

But the idea here is that the reward itself maybe never changes for certain states, which doesn't work well with the idea of gradient descent, where you continually shift and then stop when you've converged. You want to assign scores to each state, and have some way of shifting those scores.

### Contextual Bandits.

They mention this as a next step... but maybe not that important of a step. It's similar to some later stuff, but, remember, we have the problem of the action only affecting the immediate reward. Or, rather, the reward that comes from the action is the only thing that updates our estimate, NOT any indication of what state is going to come next. The dynamics are slightly different.

## Concluding Thoughts

From the conclusion...

This is really the state of the art stuff, and extends nicely, the various implementations extend nicely, to the big problems we're going to tackle in the next sections.

## Programming Exercises

- single state version of the later markov stuff
- interface to plug in how reward aggregation happens
- 10-arm testbed...
- some way of graphing the results
- exercise 2.5 - design and conduct an experiment to demonstrate the difficulties that sample-average methods have for nonstationary problems.... etc
- Optimistic initial values? Return a high score if you haven't been seen yet? A better default result? A "zero" for the aggregator!! Well... it breaks the monoid, of course, but that's okay.
- UCB action selection implementation.
- MAYBE a gradient descent thing?
- Exercise 2.11 - generate a parameter study for the nonstationary case laid out in exercise 2.5. This is going to get pretty turbo... but if I can do all this I'm going to be at the state of the art, no question about it. We'll see...

## Further Reading

- I DID read this back at Twitter... but it's not great. https://amzn.to/31thYiB
- Go find some great blog posts highlighting this stuff.
- DeepMind guys working on a book: https://tor-lattimore.com
- Book site: https://banditalgs.com
- Here's another article on bandits. https://medium.com/@tamoghnaghosh_30691/multi-armed-bandits-a-naive-form-of-reinforcement-learning-a133c8ec19be
