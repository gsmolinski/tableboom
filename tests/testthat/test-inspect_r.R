script_path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")

test_that("'inspect_r' saves file when path is passed", {
  temp_path <- tempfile(fileext = ".html")
  inspect_r(script_path, temp_path)
  expect_true(file.exists(temp_path))
})
