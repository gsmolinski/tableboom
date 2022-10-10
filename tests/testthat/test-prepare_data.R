script_path <- file.path(system.file(package = "tableboom", "example_script"), "example_script.R")

test_that("'prepare_data' returns correctly prepared
          original source code with inspected output", {
  expected <- structure(list(line = c("1", "3", "5", "7\n8\n9", "11\n12\n13",
                                      "15\n16\n17", "19", "21\n22\n23", "25", "27", "29", "31\n32\n33",
                                      "35", "36"), src_code = c("library(dplyr)", "dir <- system.file(package = \"tableboom\", \"example_script\")",
                                                                "source(paste0(dir, \"/example_script_sourced_inside.R\"))",
                                                                "my_fun2 <- function(x) {\n  x + 2\n}", "my_fun3 <<- function(x) {\n  x + 2\n}",
                                                                "function(x) {\n  x + 2\n} -> my_fun4", "assign(\"my_fun5\", function(x) x + 2)",
                                                                "for (i in 1:3) {\n  cat(i)\n}", "my_df[\"x\"]", "my_df", "my_vec <- rep(my_df$x + 10, 2)",
                                                                "my_df <- my_df %>%\n  mutate(y = mean(my_vec)) %>%\n  select(y)",
                                                                "str(my_df)", "my_df"), inspected_src_code = c("<  >  library(dplyr) ",
                                                                                                               "<  >  system.file(package = \"tableboom\", \"example_script\") \n chr \"C:/!G/tableboom/inst/example_script\"",
                                                                                                               "<  source(paste0(dir, \"/example_script_sourced_inside.R\")) \n. <  >  paste0(dir, \"/example_script_sourced_inside.R\") \n.  chr \"C:/!G/tableboom/inst/example_script/example_script_sourced_inside.R\"\n. \n>  source(paste0(dir, \"/example_script_sourced_inside.R\")) ",
                                                                                                               "", "", "", "<  assign(\"my_fun5\", function(x) x + 2) \n. <  function(x) x + 2 \n. >  function(x) x + 2 \n. function (x)  \n.  - attr(*, \"srcref\")= 'srcref' int [1:8] 19 32 19 48 32 48 19 19\n.   ..- attr(*, \"srcfile\")=Classes 'srcfilecopy', 'srcfile' <environment: 0x00000206b8aad748> \n. \n>  assign(\"my_fun5\", function(x) x + 2) ",
                                                                                                               "<  >  1:3 \n int [1:3] 1 2 3", "<  >  my_df[\"x\"] \nRows: 1\nColumns: 1\n$ x <dbl> 1",
                                                                                                               "Rows: 1\nColumns: 1\n$ x <dbl> 1", "<  rep(my_df$x + 10, 2) \n. <  my_df$x + 10 \n. . <  >  my_df$x \n. .  num 1\n. . \n. >  my_df$x + 10 \n.  num 11\n. \n>  rep(my_df$x + 10, 2) \n num [1:2] 11 11",
                                                                                                               "<  my_df %>%... \n. <  select(., y) \n. . <  mutate(., y = mean(my_vec)) \n. . . <  >  mean(my_vec) \n. . .  num 11\n. . . \n. . >  mutate(., y = mean(my_vec)) \n. . Rows: 1\n. . Columns: 2\n. . $ x <dbl> 1\n. . $ y <dbl> 11\n. . \n. >  select(., y) \n. Rows: 1\n. Columns: 1\n. $ y <dbl> 11\n. \n>  my_df %>%\n     mutate(y = mean(my_vec)) %>%\n     select(y) \nRows: 1\nColumns: 1\n$ y <dbl> 11",
                                                                                                               "'data.frame':\t1 obs. of  1 variable:\n $ y: num 11\n<  >  str(my_df) ",
                                                                                                               "Rows: 1\nColumns: 1\n$ y <dbl> 11")), class = "data.frame", row.names = c(NA,
                                                                                                                                                                                          -14L))
  obj <- prepare_data(script_path)
  # we don't want to test output for function environment, as the memory address changes constantly
  # and we don't want to test path as it can be different depending on if this package is installed and where
  expect_identical(obj[-c(2, 3, 7), ], expected[-c(2, 3, 7), ])
})

test_that("'prepare_data' returns error if no parse_data returned for original script", {
  path <- tempfile("tableboom_empty_script_", fileext = ".R")
  file.create(path)
  expect_error(prepare_data(path), "R script contains no calls to inspect.")
})

test_that("'prepare_orig_script' returns correctly prepared original source code", {
  parse_data <- utils::getParseData(parse(script_path, keep.source = TRUE), includeText = TRUE)
  exprs_df <- find_exprs(parse_data)
  expected <- structure(list(line = c("1", "3", "5", "7\n8\n9", "11\n12\n13",
                                      "15\n16\n17", "19", "21\n22\n23", "25", "27", "29", "31\n32\n33",
                                      "35", "36"), src_code = c("library(dplyr)", "dir <- system.file(package = \"tableboom\", \"example_script\")",
                                                                "source(paste0(dir, \"/example_script_sourced_inside.R\"))",
                                                                "my_fun2 <- function(x) {\n  x + 2\n}", "my_fun3 <<- function(x) {\n  x + 2\n}",
                                                                "function(x) {\n  x + 2\n} -> my_fun4", "assign(\"my_fun5\", function(x) x + 2)",
                                                                "for (i in 1:3) {\n  cat(i)\n}", "my_df[\"x\"]", "my_df", "my_vec <- rep(my_df$x + 10, 2)",
                                                                "my_df <- my_df %>%\n  mutate(y = mean(my_vec)) %>%\n  select(y)",
                                                                "str(my_df)", "my_df")), class = "data.frame", row.names = c(NA,
                                                                                                                             -14L))
  expect_identical(prepare_orig_script(parse_data, exprs_df), expected)
})
