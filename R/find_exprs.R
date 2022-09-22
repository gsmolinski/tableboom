#' Find Expressions To Which Pass as an Argument to Inspect Function
#'
#' Expressions will be passed to `boomer::boom` or `dplyr::glimpse`.
#' See `find_var_calls` function for an idea where `dplyr::glimpse`
#' will be use. In other cases - `boomer::boom` will be use.
#'
#' @param parse_data object returned by `utils::getParseData()`.
#'
#' @return
#' data.frame with 4 columns:
#' - line1 - line where expr starts
#' - line2 - line where expr ends
#' - id - id of expr from `utils::getParseData()`
#' - fun - name of function (with namespace) to use on the expr,
#' currently `boomer::boom` or `dplyr::glimpse`.
#' @importFrom rlang .data
#' @noRd
find_exprs <- function(parse_data) {
  var_calls <- find_var_calls(parse_data)

  parse_data <- parse_data |>
    dplyr::filter(.data$line1 != var_calls$line1)

  other_exprs <- parse_data |>
    dplyr::filter(.data$parent == 0) |>
    dplyr::select(.data$line1, .data$line2, .data$id) |>
    dplyr::mutate(fun = "boomer::boom")

  exprs <- dplyr::bind_rows(var_calls, other_exprs)
  exprs <- exprs |>
    dplyr::arrange(.data$line1)

  exprs
}

#' Find Where in Source Code Variable is Calling
#'
#' By "calling" we mean variable not as an argument to any function, i.e.: "my_var".
#'
#' @param parse_data object returned by `utils::getParseData()`.
#'
#' @return
#' data.frame with 3 columns:
#' - line1 - line where expr starts
#' - line2 - line where expr ends
#' - id - id of expr
#' - fun - name of function (with namespace) to use on the expr,
#' because it is var call, we use `dplyr::glimpse`.
#' @details
#' The output of e.g. `boomer::boom(iris)` looks bad, so we want to use
#' `dplyr::glimpse` in such cases.
#' @importFrom rlang .data
#' @noRd
find_var_calls <- function(parse_data) {
  var_calls <- parse_data |>
    dplyr::mutate(var_call = dplyr::if_else(.data$parent == 0 & dplyr::lead(.data$parent, default = 0) == 0, TRUE, FALSE)) |>
    dplyr::filter(.data$var_call) |>
    dplyr::select(.data$line1, .data$line2, .data$id) |>
    dplyr::mutate(fun = "dplyr::glimpse")

  var_calls
}
