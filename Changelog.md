0.2.1.3 (7 May, 2019)
=====================

* Now requires criterion-0.15.*

* No longer builds with GHC < 8.

0.2.1.2 (23 May, 2018)
======================

* Allow building with criterion-0.13.* and 0.14.*

* Allow building with temporary-1.3.*

0.2.1.1 (6 February, 2018)
==========================

* Allow building with GHC 8.2.*

* Now requires streaming-0.2.*

0.2.1.0 (13 July, 2017)
=======================

* Add `compareFuncAllWith`

* Documentation improvements (especially on `baselineWith`).

0.2.0.1 (12 July, 2017)
=======================

* Accidentally deleted the type signatures of `normalForm` and
  `normalFormIO`, breaking compilation.

0.2.0.0 (12 July, 2017)
=======================

* Benchmarking results are now printed out incrementally rather than
  requiring all of them to be completed first.

* Support for using the [weigh] library to measure memory usage.

    [weigh]: http://hackage.haskell.org/package/weigh

* Can now optionally provide a list of `CompParam` values to
  `compareFunc` and related functions rather than needing to use
  `mappend` or `<>` to manually combine them all.

* Some of Criterion's command-line options are now available,
  including the ability to output a CSV file (albeit with a different
  format).

* Can now evaluate `IO`-based benchmarks.

* Add `compareFuncList`, `compareFuncAll` (as well as primed and
  `IO`-based variants) and `normalForm` (and `normalFormIO`).

* Remove `compareFuncConstraint` as it was found to not be very
  helpful and complicated the code base.  The same functionality can
  be achieved using `compareFuncList` and related functions.

    - This includes removing `SameAs` and `CUnion` as they are now no
      longer needed.

    - The type of `compareFunc` is now different, but existing code
      shouldn't needed to change.

* The `LabelTree` data structure now includes the depth of each node.

0.1.0.0 (22 May, 2016)
======================

* Initial release
