script_path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")
parse_data <- utils::getParseData(parse(script_path, keep.source = TRUE), includeText = TRUE)
temp_path <- tempfile("tableboom", fileext = ".R")
insert_fun(find_exprs(parse_data), temp_path, script_path)

test_that("'capture_output' returns list with correct result and do not change search path", {
  search_path <- search()
  parsed_mod_file <- parse(temp_path)
  parsed_orig_file <- parse(script_path)
  expected <- list("<  >  library(dplyr) ", "<  >  system.file(package = \"tableboom\", \"example_script\") \n chr \"C:/!G/tableboom/inst/example_script\"",
                   "<  source(paste0(dir, \"/example_script_sourced_inside.R\")) \n. <  >  paste0(dir, \"/example_script_sourced_inside.R\") \n.  chr \"C:/!G/tableboom/inst/example_script/example_script_sourced_inside.R\"\n. \n>  source(paste0(dir, \"/example_script_sourced_inside.R\")) ",
                   "", "", "", "<  assign(\"my_fun5\", function(x) x + 2) \n. <  function(x) x + 2 \n. >  function(x) x + 2 \n. function (x)  \n.  - attr(*, \"srcref\")= 'srcref' int [1:8] 19 32 19 48 32 48 19 19\n.   ..- attr(*, \"srcfile\")=Classes 'srcfilecopy', 'srcfile' <environment: 0x000001d8c0500990> \n. \n>  assign(\"my_fun5\", function(x) x + 2) ",
                   "<  >  1:3 \n int [1:3] 1 2 3", "<  >  my_df[\"x\"] \nRows: 1\nColumns: 1\n$ x <dbl> 1",
                   "Rows: 1\nColumns: 1\n$ x <dbl> 1", "<  rep(my_df$x + 10, 2) \n. <  my_df$x + 10 \n. . <  >  my_df$x \n. .  num 1\n. . \n. >  my_df$x + 10 \n.  num 11\n. \n>  rep(my_df$x + 10, 2) \n num [1:2] 11 11",
                   "<  my_df %>%... \n. <  select(., y) \n. . <  mutate(., y = mean(my_vec)) \n. . . <  >  mean(my_vec) \n. . .  num 11\n. . . \n. . >  mutate(., y = mean(my_vec)) \n. . Rows: 1\n. . Columns: 2\n. . $ x <dbl> 1\n. . $ y <dbl> 11\n. . \n. >  select(., y) \n. Rows: 1\n. Columns: 1\n. $ y <dbl> 11\n. \n>  my_df %>%\n     mutate(y = mean(my_vec)) %>%\n     select(y) \nRows: 1\nColumns: 1\n$ y <dbl> 11",
                   "'data.frame':\t1 obs. of  1 variable:\n $ y: num 11\n<  >  str(my_df) ",
                   "Rows: 1\nColumns: 1\n$ y <dbl> 11")
  obj <- capture_output(parsed_mod_file, parsed_orig_file)

  expect_type(obj, "list")
  expect_length(obj, 14)
  # `boomer::boom` on `assign` using on anonymous function print function attr,
  # one of which is environment memory address, so don't want to test such efemeric thing.
  # And we don't want to check against path as it can be different depending of is and where this package is installed.
  expect_equal(obj[-c(2, 3, 7)], expected[-c(2, 3, 7)], ignore_attr = TRUE)
  expect_false("package:dplyr" %in% search_path)
})

test_that("'eval_file' successfully returns output for all calls", {
  parsed_mod_file <- parse(temp_path)
  parsed_orig_file <- parse(script_path)
  expect_error(callr::r(function(x) tableboom:::eval_file(x[[1]], x[[2]]), args = list(list(parsed_mod_file, parsed_orig_file))), NA)
})

test_that("'remove_after_empty' removes everything after first empty string", {
  expect_identical(remove_after_empty(c("a", "b", "", "c", "", "", "d")), c("a", "b"))
  expect_identical(remove_after_empty(c("e", "f")), c("e", "f"))
})

test_that("'remove_named_fun' removes output if named function
          is passed to boomer::boom", {
            user_options <- get_user_options()
            suppressMessages(set_options())
            result <- boomer::boom(fun <- function(x) x) |> utils::capture.output()
            expect_null(remove_named_fun(result))
            suppressMessages(restore_options(user_options))
          })
