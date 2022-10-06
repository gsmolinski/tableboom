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
    if (is.na(path_to_inspect) || length(path_to_inspect) > 1 || !is.character(path_to_inspect)) {
      stop("Argument passed to 'path_to_inspect' must be character length 1 or NULL.", call. = FALSE)
    } else if (tools::file_ext(path_to_inspect) != "R") {
      stop("Argument passed to 'path_to_inspect' must be path to file with 'R' extension.", call. = FALSE)
    }
  }

  if (!is.null(path_to_save)) {
    if (is.na(path_to_save) || length(path_to_save) > 1 || !is.character(path_to_save)) {
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
