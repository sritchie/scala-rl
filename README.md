# Functional RL in Scala

[![Build status](https://img.shields.io/travis/sritchie/scala-rl/develop.svg?maxAge=3600)](http://travis-ci.com/sritchie/scala-rl)
[![Codecov branch](https://img.shields.io/codecov/c/github/sritchie/scala-rl/develop.svg?maxAge=3600)](https://codecov.io/github/sritchie/scala-rl)
[![Latest version](https://index.scala-lang.org/sritchie/scala-rl/scala-rl-core/latest.svg?color=orange)](https://index.scala-lang.org/sritchie/scala-rl/scala-rl-core)
[![Gitter](https://badges.gitter.im/ScalaRL/community.svg)](https://gitter.im/ScalaRL/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![Patreon](https://img.shields.io/badge/patreon-donate-blue.svg)](https://www.patreon.com/sritchie)

### Overview

Reinforcement Learning in Scala, the functional way.

I definitely don't need to go fully overboard... but the gold standard is to reimplement a bunch of this stuff:

https://github.com/ShangtongZhang/reinforcement-learning-an-introduction

in Scala.

## Notes

Can we write the update steps in some interesting way? Like, you have a function that you pass an action to, and eventually it returns some reward? I think so!

## Blog Series

This code supports the blog series on functional reinforcement learning.

## Get Involved

Want to contribute examples or use this stuff?

## Inspiration

- the book, Reinforcement Learning.
- https://github.com/ShangtongZhang/reinforcement-learning-an-introduction

## To File

- I'm using Rainier's version, but this is a nice article about the
probability Monad:
https://www.chrisstucchio.com/blog/2016/probability_the_monad.html
- We use ScalaFMT https://scalameta.org/scalafmt/docs/installation.html
- also, wartremover http://www.wartremover.org


## License

Copyright 2019 Sam Ritchie.

Licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
