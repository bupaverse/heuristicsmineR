# heuristicsmineR

Provides the Heuristics Miner algorithm in bupaR.

## Installation

You can install the development version of heuristicsmineR with:

``` r
remotes::install_github("fmannhardt/heuristicsmineR")
```

## Example

This is a basic usage example:

``` r
library(heuristicsmineR)
library(eventdataR)
data(patients)

m <- dependency_matrix(patients)
d <- dependency_graph(m)
print(d)
causal_map(d,m)
```

