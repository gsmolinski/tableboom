script_path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")
parse_data <- utils::getParseData(parse(path, keep.source = TRUE, ), includeText = TRUE)

test_that("'insert_fun' inserts correct functions to the correct locations
          and writes temporary file", {
            temp_path <- tempfile("tableboom", fileext = ".R")
            insert_fun(find_exprs(parse_data), temp_path, script_path)
            expect_true(file.exists(temp_path))
            expected <- c("boomer::boom(library(dplyr))", "", "boomer::boom(dir <- system.file(package = \"tableboom\", \"example_script\"))",
                          "", "boomer::boom(source(paste0(dir, \"/example_script_sourced_inside.R\")))",
                          "", "boomer::boom(my_fun2 <- function(x) {", "  x + 2", "})",
                          "", "boomer::boom(my_fun3 <<- function(x) {", "  x + 2", "})",
                          "", "boomer::boom(function(x) {", "  x + 2", "} -> my_fun4)",
                          "", "boomer::boom(assign(\"my_fun5\", function(x) x + 2))", "",
                          "boomer::boom(for (i in 1:3) {", "  cat(i)", "})", "", "boomer::boom(my_df[\"x\"])",
                          "", "dplyr::glimpse(my_df)", "", "boomer::boom(my_vec <- rep(my_df$x + 10, 2))",
                          "", "boomer::boom(my_df <- my_df %>%", "  mutate(y = mean(my_vec)) %>%",
                          "  select(y))", "", "boomer::boom(str(my_df))", "dplyr::glimpse(my_df)"
            )
            expect_identical(readLines(temp_path), expected)
          })

test_that("'insert_fun' do not save any file if no parse data", {
  temp_path <- tempfile("tableboom", fileext = ".R")
  insert_fun(find_exprs(data.frame()), temp_path, script_path)
  expect_false(file.exists(temp_path))
})
