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

# Dependency graph / matrix
dependency_matrix(patients)
# Causal graph / Heuristics net
causal_net(patients)
```

![](man/figures/patients.png)

``` r
# Efficient precedence matrix
m <- precedence_matrix_absolute(L_heur_1)
as.matrix(m)

# Example from Process mining book
dependency_matrix(L_heur_1, threshold = .7)
causal_net(L_heur_1, threshold = .7)
```

![](man/figures/L_heur_1_example.png)

``` r
# Sepsis log
causal_net(sepsis, threshold = .7)

# Convert to Petri net
library(petrinetR)
cn <- causal_net(L_heur_1, threshold = .7)
pn <- as.petrinet(cn)
render_PN(pn)
```


