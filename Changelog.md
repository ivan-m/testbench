0.2.0.0
=======

* Benchmarking results are now printed out incrementally rather than
  requiring all of them to be completed first.

* Support for using the [weigh] library to measure memory usage.

    [weigh]: http://hackage.haskell.org/package/weigh

* Can now optionally provide a list of `CompParam` values to
  `compareFunc` and `compareFuncConstraint` rather than needing to use
  `mappend` or `<>` to manually combine them all.

* Some of Criterion's command-line options are now available, though
  none to do with saving results to file.

* Can now evaluate `IO`-based benchmarks.

0.1.0.0 (22 May, 2016)
======================

* Initial release
