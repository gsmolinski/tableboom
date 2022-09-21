#' Ensure CRAN Not Complaining About Imported Package
#'
#' CRAN complaints if package is included in DESCRIPTION,
#' but not used in R files. Although we use `boomer::boom`
#' in tests, but tests are not core of package, can change,
#' so it will be safer to store `boomer::boom` here.
#'
#' @return
#' This function is never used.
#' @noRd
no_cran_note <- function() {
  boomer::boom
}

#' Get Currently Set Options For `boomer` Package
#'
#' @return
#' List with options set.
#' @noRd
get_user_options <- function() {
  list(boomer_print = getOption("boomer.print"),
       boomer_safe_print = getOption("boomer.safe_print"),
       boomer_visible_only = getOption("boomer.visible_only"))
}

#' Set Options For `boomer` Package
#'
#' @return
#' Side effect - set options for this R session.
#' @details
#' We want to use `dplyr::glimpse` for printing as
#' it looks better than other options.
#'
#' We use `safe_print` to remove emoticons displayed
#' by `boomer::boom` - rather just a visual preference, but
#' at the same time we can avoid future, non-known now problems.
#'
#' We also don't want to print invisible objects, which will
#' be helpful when using `boomer::boom` on `source()` function.
#' We don't want to disaply anything explicitly which comes from
#' `source()` - if user wants this, then can just use this package
#' on other file.
#' @noRd
set_options <- function() {
  options(boomer.print = dplyr::glimpse)
  options(boomer.safe_print = TRUE)
  options(boomer.visible_only = TRUE)
  message("Setting options:\nboomer.print = dplyr::glimpse\nboomer.safe_print = TRUE\nboomer.visible_only = TRUE\n")
}

#' Restore Previous Options
#'
#' @param user_options returned by `get_user_options`. List
#' with options for `boomer` package which was set before
#' this package affected these options.
#'
#' @return
#' Side effect - set options which was present
#' before functions from this package was used.
#' @noRd
restore_options <- function(user_options) {
  options(boomer.print = user_options$boomer_print)
  options(boomer.safe_print = user_options$boomer_safe_print)
  options(boomer.visible_only = user_options$boomer_visible_only)
  message("Previous values for options:\nboomer.print,\nboomer.safe_print,\nboomer.visible_only\nare now restored")
}

#' Suppress Any Output
#'
#' We use it before `eval_file` to suppress any output
#' from user script.
#'
#' @return
#' Side effect - nothing will be visible in the console.
#' We basically stream output to the temp file.
#' @noRd
supress_console_output <- function() {
  sink(tempfile("tableboom_sink_", fileext = ".txt"))
}

#' Restore Output
#'
#' Do not `sink()`ing anymore to the file.
#'
#' @return
#' Side effect - output will be visible in the
#' console again.
#' @noRd
restore_console_output <- function() {
  sink()
}
