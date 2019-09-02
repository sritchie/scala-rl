---
layout: docs
title:  "Functional RL Course"
section: "course"
position: 1
---

# Functional RL in Scala

{% include_relative course/tabular/warning.md %}

# Notes

This is the main page for the course! Check in periodically for updates; I'm working on the code first, then looping back to the course.

# What's this?

What is this course all about? Why get into it.

Each chapter review will actually be a showcase of some software I've built to functionally show off these goodies, based on the concepts in the books. Each will have links to other interpretations of this stuff. Kind of a literate programming deal.

And, boom, I'm actually back working here on what I'm hoping will be a wonderful demo of what I've learned!

* First step is to go through and summarize the chapters, as I've been doing. For each one I can note what I'd like to be able to program and show off. The goal is to make some sort of original contribution, while at the same time creating teaching resources. But the goal is not to go completely off the deep end with this earlier stuff.

## What is Reinforcement Learning?

Would be nice to have some examples of the types of applications that everyone knows about.

# Why am I writing this guide?

Well, the abstractions don't seem... terribly well done. They're focused on getting results; I wanted to see if by re-interpreting them in a typed language I'd gain some insight into what the core concepts were, what the interfaces are that are having their implementations swapped out as we move higher up the ladder of power.

This area is different from most of the supervised learning methods that I've dealt with in previous lives, and from unsupervised learning.

I think all the money is in the second half of the book, but the first half of the book has some wonderful abstractions.

And I think, by the way, that I need to get the code for this locked down before I make the move of starting to publish these guides.

I also want to share my excitement about the things that I've been learning! Something is driving me to absorb and then spread the love, here. Why not talk more about it?

# Notes on the Book's Introduction

The book is focused on core, online learning algorithms in reinforcement learning.

If I'm going to summarize this, the key points are...

* what kinds of problems can this type of RL solve? What is novel here?
* What is the formal, mathematical framework presented to analyze these methods?
* What is missing? What should you expect?

When you come up with a mathematical model, you have a problem on your hands. Does the way that we, humans, learn, actually work this way? Are we a special case of the model? Or are we something different, are we and the model both special cases? When you come up with an area of study you're trying to get above what we're doing, and then see what application humans actually are.

> In this book we explore a computational approach to learning from interaction. (p. 1)

Important to keep the distinction clear between the three things - a problem, a class of solution methods, and the field that studies the problem and its solution methods. The term!

What is reinforcement learning? And where did these ideas come from?

* comment on _explore / exploit_ tradeoff...
* Reinforcement learning considers an entire agent, not just some subproblem. We at the outset have the entire agent coordinating with a world, either through a model or through interaction. (How does interact with the embedded agent story?)

> One must look beyond the most obvious examples of agents and their environments to appreciate the generality of the reinforcement learning framework. (p. 4)

Okay, lots of great examples of what you might do with RL...

these all involve interaction between an agent and an environment, moving toward some goal, despite uncertainty about the environment. At some point you need feedback, but feedback might not come for a while. How do you explore in the meantime, and track what you're doing?

## Elements

What are the main elements? Goals, rewards, value, and potentially a model.

What are the limitations? I think maybe this book is not the best place to find the most troubling limitations, but maybe it is.

Are we stuffing too much into this model? Well, the book does not concern itself with what is going on with the state signal, for example. How do we get a reward? Who chooses it? And... for humans, we seem to be able to do both. So what are we missing? This is a major problem with Hadoop, etc, by the way. The biggest challenges in industry are around, how do I get my input data in a nice format, and what do I do with the results? It's very easy to write about how to process data and learn models. That's maybe the most interesting thing... but not the biggest challenge.

Levels of learning, from the Tegmark book in Life 3.0

* level 1 - learn in the DNA, over generations.
* level 2 - learn from experience
* level 3 - modify yourself!

Reinforcement learning seems to be about level 2.

But.. that is sort of arbitrary too!

Okay, tic tac toe example... how does something, a game, fit within the presented model? It's interesting that they present a game against an opponent, for their example, but then MOST of the examples in the first half, because we're dealing with online learning, maybe, are about an agent playing against an environment. How would you train a model against an opponent? That has to come later. Do they both learn at the same time?

NOTE if I code this up that this is something a little funky. This is an after-state value function model, that knows that there are symmetries... we're walking around a graph, and there are multiple ways to get to a particular state.

## Unfiled Elements

* training and running, what are the counterparts for reinforcement learning? There is prediction and control. It's an online problem, so they're a little mixed up, anyway. But it would be nice to introduce the vocab etc in a separate post before getting after it with the individual ones.
* This is a nice example of how you can solve a bigger problem by breaking it into smaller problems. Who knows how the research actually progressed? But the big thing is hard to understand, and writing a book that slowly introduces degrees of freedom is a wonderful thing.
* For all the posts, we need to talk about our need to converge. The goal is that if the environment doesn't change, we want to hone in and slowly gain more knowledge about what's happening. We also want to explore, in case the environment starts to change again, which it surely will.

## Other Resources

Books, etc... what to read?
