modify_output <- function(output) {

}

#' Eval File And Capture Output of Console
#'
#' @param temp_path path to temporary, already modified file.
#' @param script_path path to script chosen by user.
#'
#' @return
#' Output from the console, returned by `utils::capture.output`.
#' @noRd
eval_file <- function(temp_path, script_path) {
  parsed_orig_file <- parse(script_path)
  parsed_mod_file <- parse(temp_path)
  e <- new.env()
  output <- vector("list", length(parsed_mod_file))
  for (i in seq_along(parsed_orig_file)) {
    try(eval(parsed_orig_file[[i]], envir = e))
    output[[i]] <- get_output(parsed_mod_file[[i]], envir = e)
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
