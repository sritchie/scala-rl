---
layout: docs
title:  "Tabular Methods"
section: "course"
---

# Tabular Methods

{% include_relative tabular/warning.md %}

# Notes on Part 1: Tabular Solution Methods

This subsection covers the first half of the RL book. I'll update this as I figure out more about how we want to structure the course.

For each of these, at first, I don't think I need to get so turbo into exactly what is going on. Just flesh out what we build up to and why it's an important idea, then make a list of what I might want to code here.

I think when I do my programming exercises, I want to note... what exactly are we building up to, here? What are we making?

The endgame is what's presented in chapter 8. The book nicely layers various concerns, but I think because of that loses the idea that the interfaces are common between all of the ideas.

SO I think I need to implement each of these things, and then go back and fill in the various implementations. What's the simplest way we could implement blah? How do we evaluate this stuff?

Then how do we make it all more complex?

Is there a way to collect stats as we go ahead and train? We're training... but what else might we want to know?

These are just great examples of how I'm able to think about research engineering problems, I think.

The overview of this section is:
* how do we learn anything at all when there's a single state, let alone multiple? Turns out this is an area called "multi-armed bandits". This is an overview that we maybe used at Twitter to deploy models. If you can get realtime feedback (like with an ads system!) you can use a system like this.
* Then we expand quickly out to multiple states and apply the markov decision process framework, which we then use for the rest of the tutorial / book.
* Next, chapters 4-6, go into dynamic programming, monte carlo methods, and then, finally, temporal-difference learning. These are all versions of the same method - play a game to conclusion.
* Then, the final two chapters go into how to combine these various methods. I don't remember what is happening at those later steps, but I'm excited to go back and find out! Chapter 8 is a full-on combination... and, I think, uses the combination method to create a nice summary of how to combine all of these methods.
