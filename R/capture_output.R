#' Capture And Modify Output From `boomer::boom` or `dplyr::glimpse`
#'
#' @param parsed_mod_file parsed (`parse()`) modified file (script).
#' @param parsed_orig_file parsed (`parse()`) original file (script).
#'
#' @return
#' List: captured and modified output for each call (each element
#' corresponds to each call in the same order as in script). For
#' each element - character vector length 1 with indicated line
#' break (`\n`). If element was NULL, then returns `""`.
#' @noRd
capture_output <- function(parsed_mod_file, parsed_orig_file) {
  output <- callr::r(function(x) {
    tableboom:::set_options()
    tableboom:::eval_file(x[[1]], x[[2]])
    }, args = list(list(parsed_mod_file, parsed_orig_file)))
  output <- lapply(output, remove_after_empty)
  output <- lapply(output, remove_named_fun)
  output <- lapply(output, paste0, collapse = "\n")
  output
}

#' Eval File And Capture Output of Console
#'
#' @param parsed_mod_file parsed (`parse()`) modified file (script).
#' @param parsed_orig_file parsed (`parse()`) original file (script).
#'
#' @return
#' List: output from the console, returned by `utils::capture.output`.
#' Each element in list is an captured output for each call in script.
#' @details
#' `boomer::boom` do not assign value to object, so if some call uses
#' object defined in previous call, this object won't exist if we would
#' just wrap all calls into `boomer::boom`. To avoid this problem, we
#' at first evaluate previous original expression (i.e. not wrapped into
#' `boomer::boom`) and then eval modified expression.
#' @noRd
eval_file <- function(parsed_mod_file, parsed_orig_file) {
  e <- new.env()
  output <- vector("list", length(parsed_mod_file))
  for (i in seq_along(parsed_orig_file)) {
    try(eval(parsed_orig_file[[i - 1]], envir = e), silent = TRUE)
    output[[i]] <- tableboom:::get_output(parsed_mod_file[[i]], envir = e)
  }
  output
}

#' Capture the Output of Evaluated Exprs
#'
#' Evaluation will continue even if error happens.
#' This will give the user whole story about the code.
#' This is helper function for `eval_file`.
#'
#' @param parsed_file modified, temp file parsed (`parse()`).
#' @param envir environment in which eval the file.
#'
#' @return
#' Captured output from the console.
#' @noRd
get_output <- function(parsed_file, envir) {
  utils::capture.output(try(eval(parsed_file, envir = envir), silent = TRUE))
}

#' Remove Empty Line And Everything After First Empty Line
#'
#' @param output output returned by `eval_file`, i.e. character vector.
#'
#' @return
#' If argument (character vector) contains empty string, returns
#' all strings before empty string, otherwise unmodified argument.
#' @details
#' It looks like output from `utils::capture.output(boomer::boom())`
#' may contain printed object even if as a print fun `dplyr::glimpse`
#' is used. Luckily, in these cases empty line precedes this printed
#' object, so we can remove it based on empty line.
#' @noRd
remove_after_empty <- function(output) {
  empty <- which(output == "")
  if (length(empty) > 0) {
    relevant_output <- which(stringi::stri_detect_fixed(output, ">"))
    if (length(relevant_output) > 0) {
      empty <- empty[empty > max(relevant_output)]
      if (length(empty) > 0) {
        empty <- min(empty)
        before_first <- empty[[1]] - 1
        output <- output[1:before_first]
      }
    }
  }
  output
}

#' Remove `boomer::boom`ed Named Functions
#'
#' @param output returned by `eval_file`, i.e. character vector. But
#' already modified by `remove_after_empty`.
#'
#' @return
#' NULL if named function was find in the output from
#' `boomer::boom`, otherwise unmodified output.
#' @details
#' We don't need to `boomer::boom` functions, because it won't display
#' anything useful, just the body of function / environment etc.
#' @noRd
remove_named_fun <- function(output) {
  if (grepl("<\\s+function", output[[1]], perl = TRUE)) {
    NULL
  } else {
    output
  }
}
