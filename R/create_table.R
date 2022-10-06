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
    tab_header("", subtitle = html(glue::glue("<em>{basename(script_path)}</em>"))) |>
    tab_source_note(html(glue::glue("<div class = 'source_note'>{basename(script_path)}</div>"))) |>
    text_transform(cells_body(),
                   fn = \(e) lapply(e, \(x) html(stringi::stri_replace_all_fixed(x, "\n", "<br/>")))) |>
    text_transform(cells_body(.data$line, which(.data$line != "")),
                   fn = \(e) lapply(e, \(x) html(glue::glue("<div class = 'line'>{x}</div>")))) |>
    text_transform(cells_body(.data$code, which(.data$line != "")),
                   fn = \(e) lapply(e, \(x) html(highlight_syntax(x)))) |>
    text_transform(cells_body(.data$code, which(.data$line == "")),
                   fn = \(e) lapply(e, \(x) clean_output(x, "<br/>"))) |>
    text_transform(cells_body(.data$code, which(.data$line == "")),
                   fn = \(e) lapply(e, \(x) html(insert_div(x, "<br/>")))) |>
    opt_align_table_header("right") |>
    cols_align("center", .data$line) |>
    opt_table_font(google_font("Fira Code")) |>
    opt_table_lines("none") |>
    opt_table_outline(color = "#fccfcf", width = "1px") |>
    opt_css(css = add_css()) |>
    tab_options(column_labels.hidden = TRUE,
                table.font.color = "#2f4f4f")
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

stylize_content <- function(transformed_data) {
  transformed_data |>
    dplyr::mutate(dplyr::across(.fns = stringi::stri_replace_all_fixed,
                                pattern = "\n", replacement = "<br/>"),
                  line = ifelse(.data$line != "", glue::glue("<div class = 'line'>{.data$line}</div>"), .data$line),
                  code = ifelse(.data$line != "", highlight_syntax(.data$code), .data$code),
                  code = ifelse(.data$line == "", clean_output(.data$code, "<br/>"), .data$code),
                  code = ifelse(.data$line == "", insert_div(.data$code, "<br/>"), .data$code))
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
    stringi::stri_replace_all_fixed("$", "<span class = 'select_code'>$</span>")
}

#' Remove Dots From The Beginning Of Output
#'
#' @param code_output output from inspect function
#' (`boomer::boom` or `dplyr::glimpse`) after modifications,
#' i.e. character vector length 1 currently stored in table.
#' @param split_sign character vector to split by.
#'
#' @return
#' Character vector with removed dots at the beginning.
#' @noRd
clean_output <- function(code_output, split_sign) {
  code_output <- code_output |>
    stringi::stri_split_fixed(split_sign) |>
    unlist(use.names = FALSE) |>
    stringi::stri_replace_all_regex("^\\.", " ")

  while (any(stringi::stri_detect_regex(code_output, "^\\s+\\."))) {
    code_output <- stringi::stri_replace_all_regex(code_output, "(^\\s+)\\.", "$1 ")
  }

  code_output <- stringi::stri_replace_all_regex(code_output, "^\\s*$", "")

  code_output <- paste0(code_output, collapse = split_sign)
  code_output
}

#' Insert 'div' Tags
#'
#' @param code_output output from inspect function
#' (`boomer::boom` or `dplyr::glimpse`) after modifications,
#' i.e. character vector length 1 currently stored in table.
#' @param split_sign character vector to split by.
#'
#' @return
#' Character vector with 'div' tags added and proper class.
#' @details
#' Divs are necessary to change style. We want to add borders
#' (different colors depending on output object, e.g. chr, num)
#' and background color for the whole block of output.
#' @noRd
insert_div <- function(code_output, split_sign) {
  code_output <- paste0("<div class = 'output_whole'>", code_output, "</div>")
  #code_output <- stringi::stri_replace_all_regex(code_output, "<br/>(\\s*<?\\s*>.+)<br/><br/>", "<br/><div class = 'output_segment'>$1</div><br/><br/>")


  code_output <- paste0(code_output, collapse = split_sign)
  code_output
}
