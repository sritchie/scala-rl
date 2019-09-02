---
layout: docs
title:  "Temporal Difference Learning"
section: "course"
---

# Temporal Difference Learning

{% include_relative warning.md %}

# Notes

Woah, getting the good stuff in, finally on to chapter 6 of the book. Temporal-difference learning.

So how did we proceed? Give me the overview again.

* What if we want to go faster and take advantage of our other estimates... can we bootstrap up? Yes, we can, by walking through the game and updating as we go, instead of storing a monster trajectory.
* First we deal with prediction, and the concept of the error introduced by doing this method...
* Then we introduce some extensions. There is this idea of batch updating, which actually is quite nice.
* Then we have expected Sarsa, the off-policy control version of this, which is sort of like value iteration.
* Q-Learning and Sarsa, and DOUBLE sarsa, are both nice examples here.
* Then we finally talk about after-states, which belongs here because it's another example of looking forward and bootstrapping knowledge.

## Sketch of the Chapter

This is the one central, novel idea to reinforcement learning, apparently! Novel because they didn't take it from older stuff, older research in psychology and dynamic systems.

I think that temporal difference and n-step are sort of the same... n-step is just more than a single step, which I think is the issue here.

As usual, first we:
* deal with the prediction problem for some arbitrary policy
* then we talk about control... finding an optimal policy. Which is both improvement and prediction at once, of course.
* MOSTLY the differences here between TD, DP and monte carlo methods are just differences in prediction. If you give me an episode I can go implement the various updates, and we'll see what happens.

## Prediction

Okay, so we start with a sketch of the monte carlo method... BUT with a constant step size for the update! that is not what was presented in the previous chapter. When they do the average they're not using a constant step size.

This new method, instead of working our way super far into the future, to the end of the episode... we only go a single step forward into the future. So of course we could use this on continuing tasks, if the state space is not so enormous.

## TD Error

This is a concept they want us to keep talking about! Monte Carlo error is the big sum of it all. Exercise 6.1 talks about how to calculate the difference between the two in the case where you're constantly updating the bullshit.

The sarsa section has us calculating (in ex. 6.8) a version of this for on-policy TD control... a version of the errors for action-value instead of state-value.

### Advantages of TD Prediction Methods

Why is this better? We're still model-less...

Over Monte Carlo, it's actually wonderful to be able to go online, in fully incremental fashion, and not to have to wait until the end of an episode.

You could totally do a ton of monte carlo, and then switch back and forth, extending your horizon, shrinking it back, and track the error, right?

AH, and the monte carlo methods in off-policy are stuck at the tail during training, as we talked about. Hard to deal with experimental methods.

Apparently they converge too, so we're good to go here. More discussion in chapter 9?
Basically this is the way to go...

### Batch Updating (Optimality of TD(0))

Okay, well, since we're only getting a SINGLE state update, and we're bootstrapping, sometimes we want to milk the hell out of our examples by processing each batch of training data... by training on each example many times until our value estimates converge.

Question about batch reinforcement learning: https://stats.stackexchange.com/questions/297708/batch-reinforcement-learning-algorithm-example

But I don't think this is quite what they're proposing... I can go ahead when I'm done writing this, go back and answer that question more completely.

one way would be to keep sampling episodes and updating.

And maybe a paper? http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.229.787&rep=rep1&type=pdf

WELLLLL actually we can just go to the python code to figure out what the hell is going on: https://github.com/ShangtongZhang/reinforcement-learning-an-introduction/blob/master/chapter06/random_walk.py#L132

Yeah, they want you to first accumulate all of the potential returns, JUST the sum piece, from all of the various episodes - update the batch - then go back and do it all again.

I wonder if there is any advantage to doing the actual bootstrapping thing, and _sampling with replacement_ from all of our trials?

Example 6.4 shows that you're much better off going with TD(0), because with limited data, especially you get to learn from estimates of states you've already visited; you're assuming that there is a markov process going on, so why not learn from that experience?

Okay, now I get it. We end with a discussion of what is going on, why it's better to get batch TD(0) than to do batch monte carlo. But I think that is extra credit, and not necessary for my goals.

#### Go into Exercise 6.7

Off-policy example here.

### Sarsa: On-Policy TD Control

Not going to be that difficult, iirc. First we need to learn action-value, not state-value... which means you need to pick TWO actions to know the value of the state.

S-A-R-S-A

Pick your first action from the given state... get a reward and new state, and use your policy to pick the next action. Sarsa.

Same trick as before required for epsilon-greedy, so that we explore all states.

BOOM we can have an action-value version of the error from before!

### Q-Learning - Off-policy TD Control

Okay, this shows up all over the place, this name, Q-Learning. What is it? What's going on here?

VERY similar to value iteration, page 82-83, where we're just always updating the best policy... except we're only using the best policy for the SECOND action choice, not for the first action choice.

So it's off-policy in that it's learning for an optimal target... but you still have to make sure that the policy from which you're sampling trajectories is soft.

Algo is on page 131.

### Expected Sarsa

Sarsa used the same policy to grab the next step. Q-learning takes the max. What about... using the expected value for the next one? This lets us get some bonus miles by moving deterministically in the same way that Sarsa moves in expectation. Great! We're basically taking all of the updates at once, that we'd get from running Sarsa a bunch of times. Why not do the best you can?

This eliminates the variance due to the random selection in Sarsa.

> In these cliff walking results Expected Sarsa was used on-policy, but in general it might use a policy di↵erent from the target policy $\pi$ to generate behavior, in which case it becomes an on-policy algorithm. For example, suppose $\pi$ is the greedy policy while behavior is more exploratory; then Expected Sarsa is exactly Q-learning. In this sense Expected Sarsa subsumes and generalizes Q-learning while reliably improving over Sarsa. Except for the small additional computational cost, Expected Sarsa may completely dominate both of the other more-well-known TD control algorithms. (p. 134)

Okay, fully badass, so good. These are nice illustrations of the good stuff... and it's all going to become more clear when I start coding, here. There is a ton to do, a ton!

### Maximization Bias and Double Learning

Monster maximization bias at hand, because each of the estimates is maximizing some random noise (at first, for example) to come up with their OWN estimates, and then we maximize across that... so we're not really taking the average, and we get a big bias upward toward the max.

Exercise 6.7 gives a nice example.

Are there ways to avoid this? yes... of course!

Okay, go into the double learning algorithm, analogous to Q-learning... there are so many algos in this book.

The algo is laid out here... if there is a second parameter, we're doing double q learning. https://github.com/ShangtongZhang/reinforcement-learning-an-introduction/blob/4d8928bb7538d81b818267c983f3fd004ffc9068/chapter06/maximization_bias.py#L69

This is a way to learn TWO estimates and use them for each other... but then when we go to actually implement the behavior we want to average both.

## Games, Afterstates and Other Special Cases

afterstate value function... we have to go back and look at tic-tac-toe, since there are multiple ways of getting to a particular state.

Woah, is there some way to get THIS into my model? To store all of the states based on how they look... under rotation? Under symmetry or something?

tic-tac-toe in the beginning was like this.

OHHHHH I see now. We can look ahead a single time, and see what state the board is going to be in right after. We have this tricky thing where we have to wait for the other person to do their move, but we know what the board is going to look like after we do our move. That is interesting and different!

Exercise 6.14 talks about after-states. I think that's just a model where we can get back to states from different ways... so, if you have some way of uniquely identifying a state, of building your graph, you should do that. (See the previous paragraph. It's not quite that simple.)

## Programming Exercises

* TD error calculation, running? Can we make an error combinator? See page 121 for examples here.
* Monte Carlo error is the big one, the sum of it all.
* Exercise 6.7 is an off-policy version of the TD(0) update... gotta do that!! It uses an importance sampling ratio, of course.
* windy gridworld!
* extensions to windy gridworld, using king's moves and stochastic wind. Gotta do this too.
* the batch version of the TD(0) and monte carlo, based on my new understanding.
* The random walk task!
* cliff-walking
* make a DOUBLE value estimator that is using two internally! another combinator! Shouldn't have to change any of the code...
