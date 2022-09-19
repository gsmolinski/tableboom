#' Insert Function To the Temporary File
#'
#' Will insert one of two inspect functions: `boomer::boom()` or `dplyr::glimpse()`.
#'
#' @param exprs_df data.frame returned by `find_exprs()` or NULL.
#' @param temp_path path to temporary location (exists as long as R session).
#' @param script_path path to script chosen by user.
#'
#' @return
#' Used for side effect - adds inspect function to the source code and writes
#' this modified source code to the temporary location.
#' @noRd
insert_fun <- function(exprs_df, temp_path, script_path) {
  file <- readLines(script_path)
  for (i in seq_along(rownames(exprs_df))) {
    file[exprs_df[i, "line1"]] <- paste0(exprs_df[i, "fun"], "(", file[exprs_df[i, "line1"]])
    file[exprs_df[i, "line2"]] <- paste0(file[exprs_df[i, "line2"]], ")")
  }
  writeLines(file, temp_path)
}
