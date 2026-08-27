
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAIVEtools

<!-- badges: start -->

<!-- badges: end -->

The goal of DAIVEtools is to support Decision Analysis for Intervention
Value Efficiency (DAIVE) by providing R functions to easily format data,
visualize results, and identify frontiers of efficiency for factorial
optimization trial data.

## Installation

You can install the development version of DAIVEtools from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("setheaton/DAIVEtools")
```

## Example

This is a basic example which shows how DAIVEtools can format posterior
draws data into a table of expected value:

``` r
library(DAIVEtools)

## combine draws dataframes into a list object
draws <- list(gpaDraws, actDraws)

## define vectors of components and outcomes labels
components <- c("A", "B", "C")
outcomes <- c("GPA", "ACT")

## generate a daive_grid object with expected values of alternatives
g <- DAIVEtools::daive_grid(draws, 
                            outcomes, 
                            components, 
                            costs=c(70, 60, 20))
get_table(g)
#>    A  B  C   names           GPA      ACT GPA.scale ACT.scale cost
#> 1 -1 -1 -1 All Off -0.3040604748 20.48321 0.3081388 0.3682862    0
#> 2 -1 -1  1       C -0.3465240718 19.08439 0.2924020 0.2585469   20
#> 3 -1  1 -1       B -0.6093514149 26.26621 0.1949990 0.8219713   60
#> 4 -1  1  1      BC -0.7063634017 25.46780 0.1590467 0.7593349   80
#> 5  1 -1 -1       A  0.6934287575 22.11390 0.6778051 0.4962159   70
#> 6  1 -1  1      AC  1.1525875078 19.09639 0.8479678 0.2594882   90
#> 7  1  1 -1      AB -0.0006719755 25.89375 0.4205736 0.7927507  130
#> 8  1  1  1     ABC  0.1605857994 23.92984 0.4803352 0.6386794  150
#>   value_function
#> 1      0.3382125
#> 2      0.2754744
#> 3      0.5084851
#> 4      0.4591908
#> 5      0.5870105
#> 6      0.5537280
#> 7      0.6066622
#> 8      0.5595073
```

Grid objects can be updated to easily combine outcomes with value
functions and identify frontiers of efficiency:

``` r
## define weights for outcomes
weights <- c(100/160, 60/160)

## calculate a value function as a weighted sum
g <- DAIVEtools::update_weights(g, weights)
summary(g)
#> DAIVE grid object with 2 outcomes and 3 components.
#> Outcomes: GPA; ACT 
#> Components: A, B, C 
#> Expected Values Table: 
#>    A  B  C   names           GPA      ACT GPA.scale ACT.scale cost
#> 1 -1 -1 -1 All Off -0.3040604748 20.48321 0.3081388 0.3682862    0
#> 2 -1 -1  1       C -0.3465240718 19.08439 0.2924020 0.2585469   20
#> 3 -1  1 -1       B -0.6093514149 26.26621 0.1949990 0.8219713   60
#> 4 -1  1  1      BC -0.7063634017 25.46780 0.1590467 0.7593349   80
#> 5  1 -1 -1       A  0.6934287575 22.11390 0.6778051 0.4962159   70
#> 6  1 -1  1      AC  1.1525875078 19.09639 0.8479678 0.2594882   90
#> 7  1  1 -1      AB -0.0006719755 25.89375 0.4205736 0.7927507  130
#> 8  1  1  1     ABC  0.1605857994 23.92984 0.4803352 0.6386794  150
#>   value_function
#> 1      0.3306941
#> 2      0.2797063
#> 3      0.4301136
#> 4      0.3841548
#> 5      0.6097091
#> 6      0.6272879
#> 7      0.5601400
#> 8      0.5397143
#> Value Function: V = 0.625 * (GPA) + 0.375 * (ACT) 
#> 
#> Alternatives on the frontier: All Off, A, AC
```

Grid objects can also easily be used to visualize frontiers of
efficiency:

``` r
DAIVEtools::frontier_plot(g, static=TRUE)
```

<img src="man/figures/README-frontier_plot-1.png" alt="" width="100%" />

And to visualize intervals for expected values:

``` r
DAIVEtools::ev_plot(g)
#> [[1]]
```

<img src="man/figures/README-ev_plot-1.png" alt="" width="100%" />

    #> 
    #> [[2]]

<img src="man/figures/README-ev_plot-2.png" alt="" width="100%" />

    #> 
    #> [[3]]

<img src="man/figures/README-ev_plot-3.png" alt="" width="100%" />
