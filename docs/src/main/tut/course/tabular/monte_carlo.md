---
layout: docs
title:  "Monte Carlo Methods"
section: "course"
---

# Monte Carlo Methods

{% include_relative warning.md %}

# Notes

There is so damned much going on in this chapter. This chapter introduces so many of the ideas that come into play later.

Okay! This chapter had my favorite exercise, and a few fun research extensions to the good stuff in it. Build up, talk about what is going on and then make that happen.

Do I want to go through and list all of the pros and cons of these methods? That is something maybe the text book can cover... I can just go into what exactly is happening. But might be nice to talk about when you'd use this style of method vs others.

* learn from actual experience
* learn from simulated experience
* you can focus in on a subset of states much more easily... since the estimates are independent for each state, you don't HAVE to go update the other states.
WE NEED TO CONVERGE! Continue to talk about this problem!

## Sketch of the chapter

We are building up all of these sections with SOME assumption that can let us cool our jets on the bigger problem. Here, we have this exploring starts idea that lets us go greedy... then we see if we can relax it.

Bullet points for how this chapter develops:

* We learned about dynamic programming. What if we no longer have the dynamics, but we get to interact with the environment directly? (Or if we have a sample model, but not a full model that lets us actually explore the full distribution?) Model-free
* In fact, we can just play the game a bunch of times... as long as we visit all states, as before, we can be sure that in the infinite case we'll eventually converge to the proper state values for all states. In fact, as long we keep exploring, the results of the generalized policy improvement theorem still apply. We can still converge by taking sample averages.
* First, prediction - getting the value function. ACTUALLY we want to start estimating action-value functions. If you have this you can easily back out the state-value function.
* Play a ton of games and average! Oh, wow, we really want an aggregator interface here.
* We are NOT bootstrapping. We are playing all the way to the end, unlike in the Dynamic Programming examples. Prediction totally works, it totally converges, if we do exploring starts as a way to make sure that we're covering all state-action pairs (since we're dealing with ANY policy here, we can't enforce anything about it).
* Now, improvement? How do we modify the policy? Really we do what we did before. As long as we have exploring starts, we can make a greedy policy by just changing the policy after every episode. No problem.
* If we remove the exploring starts, we have to keep the policy exploring, so we can get to the best possible epsilon-soft policy. But this is probably not going to be the best.
* SOMETHING has to explore, so we can either do it by exploring starts, OR by trying to get some other policy to do the learning for us, and get the information merged back in.
* This happens with importance sampling, usually. That allows us finally to be free and start updating our target policy, which, when we ACTUALLY go to play, we can decide to make just greedy, knowing that some partner is going around exploring for us.
* Then we get into the research topics, which are just ways to improve this final result.
* Where is the obvious next place to go? Can we combine the two, so we're learning from experience... but also bootstrapping?

## Notes

this chapter is about monte carlo programming - what question are we trying to answer this this new method? How are we expanding out the paradigm we saw in the previous chapter on dynamic programming?

This is what happens when we relax the idea that we have the dynamics of the environment in hand - that we know exactly how to walk around the environment model from a given state. What if we have to actually interact to "get" that information, by just seeing what the environment does?

> Monte Carlo methods require only experience—sample sequences of states, actions, and rewards from actual or simulated interaction with an environment. Learning from actual experience is striking because it requires no prior knowledge of the environment’s dynamics, yet can still attain optimal behavior. Learning from simulated experience is also powerful. Although a model is required, the model need only generate sample transitions, not the complete probability distributions of all possible transitions that is required for dynamic programming (DP). In surprisingly many cases it is easy to generate experience sampled according to the desired probability distributions, but infeasible to obtain the distributions in explicit form. (p. 91)

So now we're model free... OR we just have a super simple model that we're working with.

Limitations?

Here, for the sake of simplicity, we say that we're only going to deal with episodic tasks - not continuing. We'll extend to continuing tasks down the road. Again, trying to keep things simple.

Monte carlo methods have to wait until the END of an episode to propagate updates backward. This is analogous to the initial thing we learned about, where we wait for an entire sweep of the state space... it's a hint that maybe this can be relaxed too, but let's see.

*This is kind of like bandit methods!!* Except that now we're dealing with lots of interrelated bandit problems and propagating information among them.

* Prediction
* Improvement
* Control

The usual framework that we'll follow throughout the book. I like the framework, I think.

### Prediction

This is the problem of, given some policy, coming up with a value estimate for that policy. You can just play a ton of games and learn from your experience, instead of having to have ANY model at all. If you do have a model, then you can use it to generate simulated experience... but then, of course, "the map is not the territory", so you have to have some way of updating your model of the world. Preview of chapter 8.

The initial version is building up a monster list of returns, then averaging each one every time. So you go ahead and play using a policy, and you accumulate information, slowly, over time.

(*first opportunity for a nice interface, for an aggregator.* You have a ton of info coming in and you want to aggregate it... Boom, we know how to handle that!)
STAR - first-visit vs every-visit... come back to this later, but I think later we end up using "every visit" when we do the n-step stuff.

* blackjack example shows off nicely how to do this, since the runs are short and we can back-propagate the info, no problem. Discuss why this would be sort-of-annoying to do in a dynamic programming context... you sort of could calculate the model of the environment. (And, in fact, maybe I should do that to show off that it would be difficult)

*Monte Carlo estimates do not bootstrap!* We're not using any information from any of the other estimates. But that is a hint about where we could go from here...

The computational expense of estimating the value of a single state is independent of the total number of states... you just play from that spot, over and over.

* soap bubble example, so cool...

### Action Value Estimation

Without a model you want to start estimating actions independently, instead of letting your model store the possible actions. With a model, you can look at the model to decide what action will give you the best payoff. Without a model you have to store the actions yourself.

This is the value of following your hardcoded action, and then thereafter following your policy. Breaking the policy a single time.

### Exploring Starts

Same as in the bandit problem... if you want to keep exploring, one way to do it is to in the meta-level give yourself a nonzero chance of starting in EVERY possible state-action pair. This is the assumption of exploring starts. (p. 98)

Instead, since that's sort of unreasonable, you can just consider policies where the chance of taking a specific action can NEVER go to zero. You could do it if you wanted! But you're not guaranteed anymore to converge on an optimal solution. Maybe you don't care. Engineering tradeoff!

### Control

Now we know how to go generate value estimates for a given policy. What about upgrading the policy? (This section covers "improvement" and "control".)

We already have the framework from DP of generalized policy improvement... what do we have to adjust here?

### Improvement

What exactly IS policy improvement in this case? Why would it be any different, actually? It is definitely just making the policy greedy... which points it at new promising alleys.

START SIMPLE - first we have this exploring starts thing going on.

EXCEPT I think we can't do that... we need to keep it epsilon-greedy.

WAIT no. For the moment, ASSUME exploring starts. First we're going to introduce a version that doesn't worry about epsilon-greedy methods. Because we have the actions available at a given state, we can go ahead and just do one, and the environment will return us something good.

#### assumptions

we made two assumptions:

* infinite plays
* exploring starts

to do actual control, a practical algorithm, we have to remove those assumptions. To remove the second one, maybe we could actually track the variance instead of the total change of value estimate? Basically we give up before moving back to policy improvement.

We can use this to alternate between plays and improvements:

* Monte Carlo with Exploring Starts
* Keep a running backwards total of our rewards that we're going to pass around to all of the states that we saw.
* MAKE SURE WE only do this for the first visit - the first time we see the state, action pair, average the returns that we've seen for all of the episodes (the first running total is the running total back across this current episode), update the stored state-action value and then update the policy.

### Removing Exploring Starts

Woah, this gets us into the off-policy and on-policy distinction... we have this state function and we're trying to get it to explore all over the place.

soft policies have nonzero probabilities for all of their choices. This is a way to guarantee that there is at least SOME chance of responding to shifts in the environment.

This is a problem because we will NEVER actually get to the greedy policy.. which, we've proved, is the optimal policy. This is the problem with actually interacting with the environment!

You're going to snap-modify all of the probabilities in the policy once you get feedback on the updated state. It's not a continuous change.

### Off-Policy

Before we only achieved the best policy among epsilon-soft policies... can we still find the best policy among all policies without exploring starts? We want to be guided by experience still, in some sense.

What if we had another agent going around, passing information back in? I have a different policy generating experience... can I learn from it?

### Prediction Again!

Okay, so we want to learn from some other policy. We have to show that the other policy is going to cover ALL of the cases that our policy could cover. The assumption of coverage, page 103

We're talking about prediction again because we have ANOTHER policy... neither of the policies is updating. But you could totally just update one, independent ideas.

_importance sampling_ is the key idea for how to get the info from one policy over into another.

* ordinary importance sampling vs weighted importance sampling...  in the weighted, we are weighting each example by how likely it is to occur. Discussion on page 105 of how this is SURELY BETTER, but how it has some problems with bias vs variance that I'm going to need to look up.

We switch here to the every-visit case... exercises 5.5 covers the difference.

### Incremental Prediction

We're back to prediction again, incrementally. In the bandit case we were averaging rewards. now we're averaging returns. That's fine.

I think a lot of this madness can get pushed into the specific aggregator we want to use. Maybe we'll have to split on-policy and off-policy in the code... but we definitely can deal with the weighted and normal cases in some aggregator.

The final algorithm loops for each episode, not modifying either policy.

### Off-Policy Control

Okay, we had prediction. How do we extend to control? To actually updating our policy?

Well, don't update the b policy, but update the target policy.

One problem here is that we only learn from the tails of episodes; we're learning backwards, and our target policy currently has to approach either from the greedy solution, or it has to approach from a state that is SO GOOD that right in the moment, it overrides the existing one.

So the only way to override is to end up in a state so often that you use the entire episode to bump that state's value at the tail. Only then can you keep updating as you roll backward.

Is this a problem? Who the fuck knows! This hasn't been covered that much.

## Research Extensions

These are the big boy ideas.

### Discounting-Aware Importance Sampling

huge amount of reduction in variance IF you decide that the running sum you're keeping could in fact be discounted too. It can only have so much effect on each state. Why not take that into account?

This whole flat partial returns thing recasts the problem to show that the flat partial returns are what we actually care about, with some probability of termination at each.

(This is its own post, I think.)

By casting the thing as the sum of the flat partial returns, we can show that each flat partial return should ONLY be paired with the weight up to that point! No need to go farther! You still have to track that business...

#### Exercise 5.14

This was so good...

Gotta get into problem 5.14 and implement that shit in Scala. One badass trick... is that you can transform the denominator into a SUM that you can describe with an iterative rule. And the final term in the iterative rule can in fact be handled with the initial value of w.

But I think we can get into this later; this is a custom implementation, basically, of the other ideas in the chapter. And to understand that I first have to code up the other ideas in the chapter!

### Per-decision Importance Sampling

how is this different?

We can derive the normal one... but the key is to show that in expectation, if we remove ALL of the terms in the importance sampling product after the particular reward and do a little more work we haven't changed the expectation at all.

But with the interface provided, maybe we can experiment and track one down!

## Programming Exercises

* A world that can respond to Monte Carlo estimates. Can we make the interface the same as the simulated world? Can I just do this and then eventually get back to the code and make it look solid?
* Blackjack game is a nice place to play a ton of examples. And I can adapt my Blackjack Clojure game, maybe?
* Blackjack using Dynamic Programming? "Complex and Error Prone"... maybe not, but it certainly depends on the decks being used, etc.
* Blackjack with exploring starts?
* Blackjack with off-policy? Do we just use the same example to show off all of this bullshit?
* racetrack example.
* There has got to be a policy algebra, a way to combine these things, right? Each state is going to have to track how many times it's been seen. Can you merge them? I THINK that the importance sampling here is the key... *you can totally merge two value estimates completely...* or maybe you can if you start bootstrapping. How many levels out do you want to go when you merge? How epic is that going to be?? That would be a solid contribution again.
* Check that with tons of examples we converge to the true value with infinite data... can we do this? Can we show convergence for the normal one, in the final research topic?
