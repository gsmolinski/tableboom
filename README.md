
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tableboom

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tableboom)](https://CRAN.R-project.org/package=tableboom)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of `{tableboom}` is to let user choose the R script to inspect
intermediate results of all calls in the form of HTML table. Three main
aims are:

-   give the user possibility to inspect intermediate steps of all calls
    in the chosen R script, i.e. no need to `boomer::boom()` each call
    separately;
-   prepare output as an HTML document, so it can be read *outside* of
    programming process;
-   prepare HTML document as a table, *but* trying to make it more like
    a *word story* than table;

I have imagine situation when we have made an R script, run it, but the
result is not satisfactory. However, rather than interactively trying to
solve the problem, we would like just to *read* it as a book and think
about this outside of the IDE.

This package is intended to be a submission for **RStudio Table Contest
2022**.

## Installation

You can install the development version of `{tableboom}` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsmolinski/tableboom")
```

## Example

Having the script as below which we would like to inspect:

    library(dplyr)

    path <- file.path(system.file(package = "tableboom","table_contest_2022", "inner_script"),
                              "eurostat_data.R")

    source(path) # data - children (< 18) from Ukraine which found temp protection

    names(child_ukr_prot) <- c("country", "march_2022", "april_2022",
                               "may_2022","june_2022", "july_2022", "august_2022")

    child_ukr_prot[5, "country"] <- stringi::stri_replace_all_regex(child_ukr_prot[5, "country"],
                                                                    "\\s+.+", "")

    child_ukr_prot <- child_ukr_prot |>
      mutate(across(matches("\\d$"), \(e) if_else(e == ":", NA_character_, e)),
             across(matches("\\d$"), as.integer))

    maxs <- vector("integer", ncol(child_ukr_prot) - 1)

    for (i in 2:(ncol(child_ukr_prot))) {
      maxs[[i - 1]] <- max(child_ukr_prot[[i]], na.rm = TRUE)
    }

    maxs

    total_months <- colSums(child_ukr_prot[, 2:ncol(child_ukr_prot)], na.rm = TRUE)

    format(sum(total_months), big.mark = " ")

    path()

The usage of `{tableboom}` is nothing more than calling `inspect_r()`
with the path to the script (or `NULL` argument and then saved and
opened script in the RStudio editor will be used):

``` r
path <- file.path(system.file(package = "tableboom", "table_contest_2022"), "children_from_ukr_temp_prot_eu.R")

tableboom::inspect_r(path)
```

<img src="../../Users/gsmolinski/AppData/Local/Temp/Rtmp8YDXGc/temp_libpath447e4e615f/tableboom/help/figures/README-tableboom.png" width="100%" />
