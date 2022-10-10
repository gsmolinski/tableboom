script_path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")
prepared_data <- prepare_data(script_path)

test_that("'highlight_syntax' returns <span> tags added in correct locations", {
  expect_identical(highlight_syntax(prepared_data$src_code),
                   c("<span class = 'fun_call_code'>library</span><span class = 'bracket_code'>(</span>dplyr<span class = 'bracket_code'>)</span>",
                     "dir <- <span class = 'fun_call_code'>system.file</span><span class = 'bracket_code'>(</span>package = <span class = \"string_code\">\"tableboom\"</span>, <span class = \"string_code\">\"example_script\"</span><span class = 'bracket_code'>)</span>",
                     "<span class = 'fun_call_code'>source</span><span class = 'bracket_code'>(</span><span class = 'fun_call_code'>paste0</span><span class = 'bracket_code'>(</span>dir, <span class = \"string_code\">\"/example_script_sourced_inside.R\"</span><span class = 'bracket_code'>)</span><span class = 'bracket_code'>)</span>",
                     "my_fun2 <- <span class = 'fun_call_code'>function</span><span class = 'bracket_code'>(</span>x<span class = 'bracket_code'>)</span> <span class = 'brace_code'>{</span>\n  x + <span class = 'number_code'>2</span>\n<span class = 'brace_code'>}</span>",
                     "my_fun3 <<- <span class = 'fun_call_code'>function</span><span class = 'bracket_code'>(</span>x<span class = 'bracket_code'>)</span> <span class = 'brace_code'>{</span>\n  x + <span class = 'number_code'>2</span>\n<span class = 'brace_code'>}</span>",
                     "<span class = 'fun_call_code'>function</span><span class = 'bracket_code'>(</span>x<span class = 'bracket_code'>)</span> <span class = 'brace_code'>{</span>\n  x + <span class = 'number_code'>2</span>\n<span class = 'brace_code'>}</span> -> my_fun4",
                     "<span class = 'fun_call_code'>assign</span><span class = 'bracket_code'>(</span><span class = \"string_code\">\"my_fun5\"</span>, <span class = 'fun_call_code'>function</span><span class = 'bracket_code'>(</span>x<span class = 'bracket_code'>)</span> x + <span class = 'number_code'>2</span><span class = 'bracket_code'>)</span>",
                     "<span class = 'keyword_code'>for</span> <span class = 'bracket_code'>(</span>i <span class = 'keyword_code'>in</span> <span class = 'number_code'>1</span>:<span class = 'number_code'>3</span><span class = 'bracket_code'>)</span> <span class = 'brace_code'>{</span>\n  <span class = 'fun_call_code'>cat</span><span class = 'bracket_code'>(</span>i<span class = 'bracket_code'>)</span>\n<span class = 'brace_code'>}</span>",
                     "my_df<span class = 'select_code'>[</span><span class = \"string_code\">\"x\"</span><span class = 'select_code'>]</span>",
                     "my_df", "my_vec <- <span class = 'fun_call_code'>rep</span><span class = 'bracket_code'>(</span>my_df<span class = 'select_code'>$</span>x + <span class = 'number_code'>10</span>, <span class = 'number_code'>2</span><span class = 'bracket_code'>)</span>",
                     "my_df <- my_df %>%\n  <span class = 'fun_call_code'>mutate</span><span class = 'bracket_code'>(</span>y = <span class = 'fun_call_code'>mean</span><span class = 'bracket_code'>(</span>my_vec<span class = 'bracket_code'>)</span><span class = 'bracket_code'>)</span> %>%\n  <span class = 'fun_call_code'>select</span><span class = 'bracket_code'>(</span>y<span class = 'bracket_code'>)</span>",
                     "<span class = 'fun_call_code'>str</span><span class = 'bracket_code'>(</span>my_df<span class = 'bracket_code'>)</span>",
                     "my_df"))
})

test_that("'transform_data' returns data in expected form", {
  df <- data.frame(line = c("1", "2\n3"),
                   src_code = c("library(dplyr)", "c(1,\n2)"),
                   inspected_src_code = c("< > library(dplyr)", "num [1:2] 1 2"))

  expected <- data.frame(line = c("1", "", "2\n3", ""),
                         code = c("library(dplyr)", "< > library(dplyr)", "c(1,\n2)", "num [1:2] 1 2"))
  expect_identical(transform_data(df), expected)
})

test_that("'clean_output' removes dots and unnecessary space", {
  obj <- "<  my_df %>%... \n. <  select(., y) \n. . <  mutate(., y = mean(my_vec)) \n. . . <  >  mean(my_vec) \n. . .  num 11\n. . . \n. . >  mutate(., y = mean(my_vec)) \n. . Rows: 1\n. . Columns: 2\n. . $ x <dbl> 1\n. . $ y <dbl> 11\n. . \n. >  select(., y) \n. Rows: 1\n. Columns: 1\n. $ y <dbl> 11\n. \n>  my_df %>%\n     mutate(y = mean(my_vec)) %>%\n     select(y) \nRows: 1\nColumns: 1\n$ y <dbl> 11"
  clean_output(obj, "\n")
  expect_identical(clean_output(obj, "\n"),
                   "<  my_df %>%... \n  <  select(., y) \n    <  mutate(., y = mean(my_vec)) \n      <  >  mean(my_vec) \n       num 11\n\n    >  mutate(., y = mean(my_vec)) \n    Rows: 1\n    Columns: 2\n    $ x <dbl> 1\n    $ y <dbl> 11\n\n  >  select(., y) \n  Rows: 1\n  Columns: 1\n  $ y <dbl> 11\n\n>  my_df %>%\n     mutate(y = mean(my_vec)) %>%\n     select(y) \nRows: 1\nColumns: 1\n$ y <dbl> 11")
})

test_that("'insert_div' insert divs in correct locations", {
  obj <- "  <  c(1, 2, 3) \n   num 1 2 3\n"
  expect_identical(insert_div(obj, "\n"),
                   "<div class = 'output_whole'>  <  c(1, 2, 3) \n<span class = 'num_output'>   num 1 2 3</span>\n</div>")
})
