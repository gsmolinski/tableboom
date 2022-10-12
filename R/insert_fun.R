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
#' @details
#' We want to add code to exact location, becuse expression can end with comment or
#' in one line can be many expressions separated by `;`. In these cases we can't
#' just add something to the beginning of line (like fun with opened bracket) and
#' something to the end of line (like closed bracket), we need to analyze step-by-step
#' what we have modified so far and how the location changed.
#' @noRd
insert_fun <- function(exprs_df, temp_path, script_path) {
  file <- readLines(script_path)
  file_orig <- file
  for (i in seq_along(rownames(exprs_df))) {

    split_str <- unlist(strsplit(file[exprs_df[i, "line1"]], ""), use.names = FALSE)
    chars_added <- nchar(file[exprs_df[i, "line1"]]) - nchar(file_orig[exprs_df[i, "line1"]])
    # minus 1 below, because we want to add function "before" first character, not "after"
    str_modified <- append(split_str, paste0(exprs_df[i, "fun"], "("), after = (exprs_df[i, "col1"] - 1) + chars_added)
    file[exprs_df[i, "line1"]] <- paste0(str_modified, collapse = "")

    split_str <- unlist(strsplit(file[exprs_df[i, "line2"]], ""), use.names = FALSE)
    chars_added <- nchar(file[exprs_df[i, "line2"]]) - nchar(file_orig[exprs_df[i, "line2"]])
    str_modified <- append(split_str, ")", after = exprs_df[i, "col2"] + chars_added)
    file[exprs_df[i, "line2"]] <- paste0(str_modified, collapse = "")

  }
  writeLines(file, temp_path)
}
