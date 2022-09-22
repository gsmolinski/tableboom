script_path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")
obj <- suppressMessages(prepare_data(script_path))
obj$src_code <- highlight_syntax(obj$src_code)

test_that("'transform_data' returns data in expected form", {
  df <- data.frame(line = c("1", "2\n3"),
                   src_code = c("library(dplyr)", "c(1,\n2)"),
                   inspected_src_code = c("< > library(dplyr)", "num [1:2] 1 2"))

  expected <- data.frame(line = c("1", "", "2\n3", ""),
                         code = c("library(dplyr)", "< > library(dplyr)", "c(1,\n2)", "num [1:2] 1 2"))
  expect_identical(transform_data(df), expected)
})


