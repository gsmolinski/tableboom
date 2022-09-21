#' Prepare Data For Table
#'
#' @param script_path - path to original script, i.e. script which will be inspected.
#'
#' @return
#' tibble with cols:
#' - line - lines as a character vector length 1 (separated by `\n`)
#' - src_code - source code as a character vector length 1 (separated by `\n`)
#' - inspected_src_code - output from `dplyr::glimpse` or `boomer::boom` as
#' a character vector length 1 (separated by `\n`)
#' or Error (Error if nrow == 0 for data retrieved by `utils::getParseData()`
#' used on original script).
#' @noRd
prepare_data <- function(script_path) {
  parsed_orig_file <- parse(script_path, keep.source = TRUE)
  parse_data_orig_file <- utils::getParseData(parsed_orig_file, includeText = FALSE)
  if (nrow(parse_data_orig_file) > 0) {
    user_options <- get_user_options()
    set_options()

    exprs_df <- find_exprs(parse_data_orig_file)
    temp_path <- tempfile("tableboom_", fileext = ".R")
    insert_fun(exprs_df, temp_path, script_path)
    parsed_mod_file <- parse(temp_path, keep.source = TRUE)

    prepared_orig_script <- prepare_orig_script(parse_data_orig_file, exprs_df)

    supress_console_output()
    inspected_src_code <- suppressWarnings(suppressMessages(capture_output(parsed_mod_file, parsed_orig_file)))
    restore_console_output()

    restore_options(user_options)

    prepared_data <- prepared_orig_script |>
      dplyr::mutate(inspected_src_code = inspected_src_code)

    prepared_data
  } else {
    stop("R script contains no calls to inspect.", call. = FALSE)
  }
}

#' Get Expression Using `utils::getParseText()`
#'
#' @param parse_data data returned by `utils::getParseData()` which was used on original script.
#' @param exprs_df returned by `find_exprs`, i.e. list of exprs found in original script, as well
#' as line1, line2, id and fun to inspect (`dplyr::glimpse` or `boomer::boom`)
#'
#' @return
#' tibble - each column is a list containing data retrieved from original script:
#' - line - lines as a character vector length 1 (separated by `\n`)
#' - src_code - source code as a character vector length 1 (separated by `\n`)
#' Each row correspondents to each expression from original script.
#' @noRd
prepare_orig_script <- function(parse_data, exprs_df) {
  line <- seq_vectorized(from = exprs_df$line1, to = exprs_df$line2)
  line <- lapply(line, paste0, collapse = "\n")
  src_code <- lapply(exprs_df$id, function(e) utils::getParseText(parse_data, e))

  tibble::tibble(line = line,
                 src_code = src_code)
}

# vectorized version of seq
seq_vectorized <- Vectorize(seq.default, vectorize.args = c("from", "to"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
