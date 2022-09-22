#' Create Table
#'
#' @param script_path path to original script, will be used as table title / header.
#' @param prepared_data data to be used in table.
#'
#' @return
#' Table object.
#' @import gt
#' @noRd
create_table <- function(script_path, prepared_data) {
  table_data <- transform_data(prepared_data)

  gt_table <- table_data |>
    gt()
}

#' Transform Data Into Mode Applicable For Table
#'
#' @param prepared_data data returned by `prepare_data()`.
#'
#' @return
#' data.frame with two cols:
#' - line - lines corresponds to lines of original source code,
#' empty if inspected code
#' - code - original source code or inspected source code (line by line)
#' @details
#' Aim is to transform data from form where in separate column is inspected code
#' into the form where in one column is original code and inspected code, in the
#' form where in one row is original code, then inspected, then original etc.
#' @importFrom rlang .data
#' @noRd
transform_data <- function(prepared_data) {
  prepared_data_src_code <- prepared_data |>
    dplyr::mutate(order = seq.int(1, nrow(prepared_data) * 2, 2)) |>
    dplyr::rename(code = .data$src_code) |>
    dplyr::select(.data$order, .data$line, .data$code)

  prepared_data_inspected <- prepared_data |>
    dplyr::mutate(line = "") |>
    dplyr::mutate(order = seq.int(2, nrow(prepared_data) * 2, 2)) |>
    dplyr::rename(code = .data$inspected_src_code) |>
    dplyr::select(.data$order, .data$line, .data$code)

  transformed_data <- dplyr::bind_rows(prepared_data_src_code, prepared_data_inspected)

  transformed_data <- transformed_data |>
    dplyr::arrange(.data$order) |>
    dplyr::select(.data$line, .data$code)

  transformed_data
}
