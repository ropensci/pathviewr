
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pathviewR

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R build
status](https://github.com/vbaliga/pathviewR/workflows/R-CMD-check/badge.svg)](https://github.com/vbaliga/pathviewR/actions)
[![Travis build
status](https://travis-ci.org/vbaliga/pathviewR.svg?branch=master)](https://travis-ci.org/vbaliga/pathviewR)
<!-- badges: end -->

`pathviewR` offers tools to import, clean, and visualize animal movement
data from [Optitrack’s Motive](https://optitrack.com/products/motive/),
the Straw Lab’s [Flydra](https://github.com/strawlab/flydra), or from
other sources. We provide functions to remove artifacts, standardize
tunnel position and tunnel axes, select a region of interest, isolate
specific trajectories, fill gaps in trajectory data, and calculate 3D
and per-axis velocity. For experiments of visual guidance, we also
provide functions that use animal position to estimate perception of
visual stimuli.

## Installation

This package can be installed via:

``` r
#install.packages("devtools") # if devtools is not installed
devtools::install_github("vbaliga/pathviewR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(pathviewR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

## Citation

TBD

## License

GPL (\>= 3) + file LICENSE

🐢

Note to self: You’ll still need to render `README.Rmd` regularly, to
keep `README.md` up-to-date. Do this via `devtools::build_readme()`

Let’s see if the github action works.
