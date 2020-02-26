# heuristicsmineR 0.2.2

* Prepare integration with `processanimateR`  
* Fix occasional negative values in `performance` profile
* Added `custom` profile
* Fixed warning in  `precedence_matrix` (Thanks to @nacnudus for the PR)
* Fixed off-by-one error leading to not counting last event correctly.

# heuristicsmineR 0.2.1

* Bugfix buffer over-read in `count_length_two_loops.cpp`

# heuristicsmineR 0.2.0

* Bugfix L2 loop detection
* Lifecycle-aware heuristic `dependency_type_lifecycle`
* Performance visualisation `causal_performance`

# heuristicsmineR 0.1.3

* Bugfix for `all_connected` heuristics, which correctly ignores self-loops now.
* Added frequency threshold as described in the Process Mining book.

# heuristicsmineR 0.1.2

* Some performance improvements

# heuristicsmineR 0.1.1

* Added causal nets with frequencies and bindings
* Added L1 and L2 heuristics by Flexible Heuristic Miner

# heuristicsmineR 0.1.0

* Initial implementation (simple dependency graph only)
