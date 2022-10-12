#' Prepare Data For Table
#'
#' @param script_path - path to original script, i.e. script which will be inspected.
#'
#' @return
#' data.frame with cols:
#' - line - lines as a character vector (separated by `\n`)
#' - src_code - source code as a character vector (separated by `\n`)
#' - inspected_src_code - output from `dplyr::glimpse` or `boomer::boom` as
#' a character vector (separated by `\n`)
#' or Error (Error if nrow == 0 for data retrieved by `utils::getParseData()`
#' used on original script).
#' @noRd
prepare_data <- function(script_path) {
  parsed_orig_file <- parse(script_path, keep.source = TRUE)
  parse_data_orig_file <- utils::getParseData(parsed_orig_file, includeText = FALSE)
  if (nrow(parse_data_orig_file) > 0) {
    exprs_df <- find_exprs(parse_data_orig_file)
    temp_path <- tempfile("tableboom_", fileext = ".R")
    insert_fun(exprs_df, temp_path, script_path)
    parsed_mod_file <- parse(temp_path, keep.source = TRUE)

    prepared_orig_script <- prepare_orig_script(script_path, exprs_df)

    inspected_src_code <- capture_output(parsed_mod_file, parsed_orig_file)

    prepared_data <- prepared_orig_script |>
      dplyr::mutate(inspected_src_code = unlist(inspected_src_code, use.names = FALSE))

    prepared_data
  } else {
    stop("R script contains no calls to inspect.", call. = FALSE)
  }
}

#' Get Original Script With Lines Number
#'
#' @param script_path - path to original script, i.e. script which will be inspected.
#' @param exprs_df returned by `find_exprs`, i.e. list of exprs found in original script, as well
#' as line1, line2, id and fun to inspect (`dplyr::glimpse` or `boomer::boom`)
#'
#' @return
#' data.frame - each column contains data retrieved from original script:
#' - line - lines as a character vector (separated by `\n`)
#' - src_code - source code as a character vector (separated by `\n`)
#' Each row correspondents to each expression from original script.
#' @noRd
prepare_orig_script <- function(script_path, exprs_df) {
  line <- seq_vectorized(from = exprs_df$line1, to = exprs_df$line2)
  line_chr <- vapply(line, paste0, FUN.VALUE = character(1), collapse = "\n")
  src_code <- readLines(script_path)
  src_code <- unlist(lapply(line, collapse_src_code, src_code = src_code), use.names = FALSE)

  data.frame(line = line_chr,
             src_code = src_code)
}

#' Collapse Source Code Which Belongs to the Same Expr
#'
#' @param lines numeric vector - lines to take from src_code.
#' @param src_code script returned by `readLines`.
#'
#' @return
#' character vector length 1 - source code collapse using `\n`.
#' @noRd
collapse_src_code <- function(lines, src_code) {
  paste0(src_code[lines], collapse = "\n")
}

# vectorized version of seq
seq_vectorized <- Vectorize(seq.default, vectorize.args = c("from", "to"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
