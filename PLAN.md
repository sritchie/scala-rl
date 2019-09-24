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

## Next Steps

The next phase is about policy evaluation and improvement, and the back and forth between the two.

I sort of have this working now. I've got

- policy evaluation, which now has to BEGIN with a policy and a base, default bullshit.

I need:

- policy IMPROVEMENT, where I maximize with respect to the value. (this latter can generate a softmax, of course.)
