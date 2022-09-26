#' Create Table
#'
#' @param script_path path to original script, will be used as table title / header.
#' @param prepared_data data returned by `prepare_data()`.
#'
#' @return
#' Table object.
#' @import gt
#' @noRd
create_table <- function(script_path, prepared_data) {
  prepared_data |>
    transform_data() |>
    gt() |>
    tab_header(basename(script_path)) |>
    text_transform(locations = cells_body(),
                   fn = function(e) lapply(e, function(x) html(stringi::stri_replace_all_fixed(x, "\n", "<br/>")))) |>
    text_transform(locations = cells_body(code, which(.data$line != "")),
                   fn = function(e) lapply(e, function(x) html(highlight_syntax(x)))) |>
    opt_table_font(font = google_font("Fira Code")) |>
    opt_css(css = add_css()) |>
    tab_options(column_labels.hidden = TRUE)
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
    dplyr::mutate(line = "",
                  order = seq.int(2, nrow(prepared_data) * 2, 2)) |>
    dplyr::rename(code = .data$inspected_src_code) |>
    dplyr::select(.data$order, .data$line, .data$code)

  transformed_data <- dplyr::bind_rows(prepared_data_src_code, prepared_data_inspected)

  transformed_data <- transformed_data |>
    dplyr::arrange(.data$order) |>
    dplyr::select(.data$line, .data$code)

  transformed_data
}

#' Add HTML 'span' Tag To Highlight Syntax
#'
#' @param code source code to apply syntax highlighting on.
#'
#' @return
#' Character vector with 'span' tags added with classes.
#' @details
#' For this function crucial is to correctly set CSS rules.
#' @noRd
highlight_syntax <- function(code) {
  code |>
    stringi::stri_replace_all_regex('(["](.*?)["])', '<span class = "string_code">$1</span>') |>
    stringi::stri_replace_all_regex("(['](.*?)['])", "<span class = 'string_code'>$1</span>") |>
    stringi::stri_replace_all_regex("(\\.*[\\w.]+|`.+`)(?=\\()", "<span class = 'fun_call_code'>$1</span>") |>
    stringi::stri_replace_all_regex("(%.+%)", "<span class = 'fun_call_code'>$1</span>") |>
    stringi::stri_replace_all_regex("(\\w+:{3}|\\w+:{2})", "<span class = 'namespace_code'>$1</span>") |>
    stringi::stri_replace_all_regex("\\b((?:TRUE|FALSE|T|F|NA|NA_character_|NA_integer_|NA_complex_|NA_real_|NULL))\\b", "<span class = 'specials_code'>$1</span>") |>
    stringi::stri_replace_all_regex("\\b((?:if|else|repeat|while|for|in|next|break))\\b", "<span class = 'keyword_code'>$1</span>") |>
    stringi::stri_replace_all_regex("\\b([-+]?(0x[\\dA-Fa-f]+|\\d*\\.?\\d+([Ee]-?\\d+)?i?|Inf|NaN))\\b", "<span class = 'number_code'>$1</span>") |>
    stringi::stri_replace_all_fixed("#", "<span class = 'comment_code'>#</span>") |>
    stringi::stri_replace_all_fixed("(", "<span class = 'bracket_code'>(</span>") |>
    stringi::stri_replace_all_fixed(")", "<span class = 'bracket_code'>)</span>") |>
    stringi::stri_replace_all_fixed("{", "<span class = 'brace_code'>{</span>") |>
    stringi::stri_replace_all_fixed("}", "<span class = 'brace_code'>}</span>") |>
    stringi::stri_replace_all_fixed("[", "<span class = 'select_code'>[</span>") |>
    stringi::stri_replace_all_fixed("]", "<span class = 'select_code'>]</span>") |>
    stringi::stri_replace_all_fixed("$", "<span class = 'select_code'>$</span>") |>
    stringi::stri_replace_all_fixed("|>", "<span class = 'fun_call_code'>$1</span>")
}

