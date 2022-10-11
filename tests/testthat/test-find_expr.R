path <- file.path(system.file(package = "tableboom", "example_script"), "example_script.R")
parse_data <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)

test_that("'find_var_calls' returns correct lines where var is calling", {
  expected <- data.frame(line1 = c(27L, 36L),
                         line2 = c(27L, 36L),
                         col2 = c(5L, 5L),
                         id = c(249L, 348L),
                         fun = "dplyr::glimpse")
  expect_equal(find_var_calls(parse_data), expected, ignore_attr = TRUE)
})

test_that("'find_exprs' returns correct lines for all exprs", {
  expected <- data.frame(line1 = c(1L, 3L, 5L, 7L, 11L, 15L, 19L, 21L, 25L, 27L, 29L, 31L, 35L, 36L),
                         line2 = c(1L, 3L, 5L, 9L, 13L, 17L, 19L, 23L, 25L, 27L, 29L, 33L, 35L, 36L),
                         col2 = c(14L, 59L, 55L, 1L, 1L, 12L, 36L, 1L, 10L, 5L, 30L, 11L, 10L, 5L),
                         id = c(10L, 38L, 66L, 97L, 127L, 157L, 188L, 227L, 241L, 249L, 281L, 328L, 342L, 348L),
                         fun = c(rep("boomer::boom", 9), "dplyr::glimpse", rep("boomer::boom", 3), "dplyr::glimpse"))
  expect_equal(find_exprs(parse_data), expected, ignore_attr = TRUE)
})
