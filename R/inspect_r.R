#' Inspect Intermediate Steps of All Calls in R Script
#'
#' All calls are inspected in the chosen R script, i.e. output of intermediate
#' steps is displayed as a HTML table which can be optionally saved as `html` file.
#'
#' @param path_to_inspect path to R script which should be inspected. If `NULL` (default)
#' active file in RStudio will be used. If not `NULL`, `.R` file extension must be used.
#' @param path_to_save where to save HTML table? If `NULL` (default) table is not saved
#' and only displayed in the Viewer panel. If not `NULL`, `.html` file extension must be used.
#'
#' @return
#' HTML table and - optionally - HTML table saved as a side effect.
#' @details
#' Calls are inspected using [boomer::boom] or [dplyr::glimpse]. Please note
#' that comments in separate lines (i.e. in the line is no source code, only comment)
#' are not included in the output.
#' @export
#' @import gt
#' @examples
#' inspect_r(file.path(system.file(package = "tableboom", "table_contest_2022"),
#'                     "children_from_ukr_temp_prot_eu.R"))
#'
#' # To see original script, use:
#' # cat(paste0(readLines(file.path(system.file(package = "tableboom", "table_contest_2022"),
#' #                                "children_from_ukr_temp_prot_eu.R")),
#' #            collapse = "\n"))
inspect_r <- function(path_to_inspect = NULL, path_to_save = NULL) {
  check_args(path_to_inspect, path_to_save)
  if (is.null(path_to_inspect)) {
    path_to_inspect <- get_active_file()
  } else {
    path_to_inspect <- tools::file_path_as_absolute(path_to_inspect)
  }

  prepared_data <- prepare_data(path_to_inspect)
  table <- create_table(path_to_inspect, prepared_data)

  if (!is.null(path_to_save)) {
    gtsave(table, filename = basename(path_to_save), path = dirname(path_to_save))
  }
  table
}

#' Check If All Arguments Passed to `inspect_r` Parameters
#' Are Valid
#'
#' @param path_to_inspect path to file which will be inspected.
#' @param path_to_save where to save HTML table?
#'
#' @return
#' NULL or error.
#' @noRd
check_args <- function(path_to_inspect, path_to_save) {
  if (!is.null(path_to_inspect)) {
    if (length(path_to_inspect) > 1 || is.na(path_to_inspect) || !is.character(path_to_inspect)) {
      stop("Argument passed to 'path_to_inspect' must be character length 1 or NULL.", call. = FALSE)
    } else if (tools::file_ext(path_to_inspect) != "R") {
      stop("Argument passed to 'path_to_inspect' must be path to file with 'R' extension.", call. = FALSE)
    }
  }

  if (!is.null(path_to_save)) {
    if (length(path_to_save) > 1 || is.na(path_to_save) || !is.character(path_to_save)) {
      stop("Argument passed to 'path_to_save' must be character length 1 or NULL.", call. = FALSE)
    } else if (tools::file_ext(path_to_save) != "html") {
      stop("If path is provided as an argument to 'path_to_save', then extension must be 'html'.", call. = FALSE)
    }
  }
}

#' Get Active File in RStudio
#'
#' @return
#' Absolute path to active file in RStudio script editor
#' or error if no active file found.
#' @noRd
get_active_file <- function() {
  path <- tryCatch(rstudioapi::getSourceEditorContext()$path,
                   error = function(e) "")
  if (is.null(path) || path == "" || tools::file_ext(path) != "R") {
    stop("Can't find active and saved file in RStudio source editor with 'R' extenstion. Pass path to 'path_to_script' or make sure in RStudio source editor is active R file.", call. = FALSE)
  } else {
    path
  }
}
