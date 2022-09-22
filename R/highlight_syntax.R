#' Add HTML <span> Code To Highlight Syntax
#'
#' @param code source code to apply syntax highlighting on
#'
#' @return
#' Character vector with <span> tags added with classes.
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
