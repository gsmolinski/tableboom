
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tableboom

<!-- badges: start -->
<!-- badges: end -->

The goal of `{tableboom}` is to let user choose the R script to inspect
intermediate results of all calls in the form of HTML table. Three main
aims are:

-   give the user possibility to inspect intermediate steps of all calls
    in the chosen R script, i.e. no need to `boomer::boom()` each call
    separately;
-   prepare output as an HTML document, so it can be read when not
    programming;
-   prepare HTML document as a table, *but* trying to make it more like
    a words than table;

I have imagine situation when we have made an R script, run it, but the
result is not satisfactory. However, rather than interactively trying to
solve the problem, we would like just to *read* it as a book and think
about this outside of the IDE.

This package is intended to be a submission for RStudio Table Contest
2022.

## Installation

You can install the development version of tableboom from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsmolinski/tableboom")
```
