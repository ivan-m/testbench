testbench
=========

[![Hackage](https://img.shields.io/hackage/v/testbench.svg)](https://hackage.haskell.org/package/testbench) [![Build Status](https://travis-ci.org/ivan-m/testbench.svg)](https://travis-ci.org/ivan-m/testbench)

> Test your benchmarks!

> Benchmark your tests!

It's too easy to accidentally try and benchmark apples and oranges
together.  Wouldn't it be nice if you could somehow guarantee that
your benchmarks satisfy some simple tests (e.g. a group of comparisons
all return the same value)?

Furthermore, trying to compare multiple inputs/functions against each
other requires a lot of boilerplate, making it even easier to
accidentally compare the wrong things (e.g. using `whnf` instead of
`nf`).

_testbench_ aims to help solve these problems and more by making it
easier to write unit tests and benchmarks together by stating up-front
what requirements are needed and then using simple functions to state
the next parameter to be tested/benchmarked.

This uses [HUnit] and [criterion] to create the tests and benchmarks
respectively, and it's possible to obtain these explicitly to embed
them within existing test- or benchmark-suites.  Alternatively, you
can use the provided `testBench` function directly to first run the
tests and then -- if the tests all succeeded -- run the benchmarks.

[HUnit]: https://hackage.haskell.org/package/HUnit
[criterion]: https://hackage.haskell.org/package/criterion

Examples
--------

Please see the provided `examples/` directory.

Limitations
-----------

* No explicit `IO` or monadic support.  It might work (and there are
  some escape hatches for stating how to create `Benchmarkable`
  values) but has not been tested yet.

* No availability of specifying an environment to benchmarks in.

* _criterion_ currently does not lend itself well to visualising the
  results from this:

    - A very limited internal tree-like structure which is not really
      apparent when results are displayed.

    - No easy way to actually _compare_ benchmark values: there used
      to be a `bcompare` function (and when work on _testbench_ was
      first started it was intended to take advantage of this
      function) but it is no longer present.  As such, comparisons
      must be done by hand by comparing the results visually.

    - Having more than a few benchmarks together produces a lot of
      output (either to the terminal or a resulting report): combined
      with the previous point, having more than a few benchmarks is
      discouraged.

* The `testBench` function for running the tests and benchmarks is
  currently very stupid.  For example, there is no way to specify that
  you do not wish to run the tests from the command-line, and
  _criterion_ command-line parameters happen to be accepted purely by
  happenstance.

Fortuitously Anticipated Queries
--------------------------------

### Why write this library?

The idea behind _testbench_ came about because of two related
dissatisfactions with _criterion_ that I found:

1. Even when the `bcompare` function was still available, it still
   seemed very difficult/clumsy to write comparison benchmarks since
   so much needed to be duplicated for each comparison.

2. When trying to find examples of benchmarks that performed
   comparisons between different implementations, I came across some
   that seemingly did the same calculation on different
   inputs/implementations, but upon closer analysis the implementation
   that "won" was actually doing less work than the others (not by a
   large amount, but the difference was non-negligible in my opinion).
   This would have been easy to pick up if even a simple test was
   performed (e.g. using `==` would have led rise to a type mis-match,
   making it obvious they did different things).

_testbench_ aims to solve these problems by making it easier to write
comparisons up-front: by using the `compareFunc` function to specify
what you are benchmarking and how, then using `comp` just to specify
the input (without needing to also re-specify the function,
evaluationg type, etc.).

### Why not use hspec/tasty/some-other-testing-framework?

Hopefully by the nature of this question it is obvious why I did not
pick one over the others.  HUnit is low-level enough that it can be
utilised by any of the others if so required whilst keeping the
dependencies required minimal.

Not to mention that these tests are more aimed at checking that the
_benchmarks_ are valid and are thus typically equality/predicate-based
tests on the result from a simple function; as such it is more
intended that they are quickly run as a verification stage rather than
the basis for a large test-suite.
