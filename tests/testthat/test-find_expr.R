path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")
parse_data <- utils::getParseData(parse(path, keep.source = TRUE, ), includeText = TRUE)

test_that("'find_var_calls' returns correct lines where var is calling", {
  expected <- data.frame(line1 = c(27L, 36L),
                         line2 = c(27L, 36L),
                         fun = "dplyr::glimpse")
  expect_identical(find_var_calls(parse_data), expected)
})

test_that("'find_exprs' returns correct lines for all exprs", {
  expected <- data.frame(line1 = c(27L, 36L, 1L, 3L, 5L, 7L, 11L, 15L, 19L, 21L, 25L, 29L, 31L, 35L),
                         line2 = c(27L, 36L, 1L, 3L, 5L, 9L, 13L, 17L, 19L, 23L, 25L, 29L, 33L, 35L),
                         fun = c(rep("dplyr::glimpse", 2), rep("boomer::boom", 12)))
  expect_identical(find_exprs(parse_data), expected)
})

test_that("'find_exprs' returns NULL if no parse data", {
  expect_null(find_exprs(data.frame()))
})
