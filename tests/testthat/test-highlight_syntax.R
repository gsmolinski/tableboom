script_path <- paste0(system.file(package = "tableboom", "example_script"), "/example_script.R")

test_that("'highlight_syntax' returns <span> tags added in correct locations", {
  obj <- suppressMessages(prepare_data(script_path))
  expect_identical(highlight_syntax(obj$src_code),
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
                     "my_df <- my_df <span class = 'fun_call_code'>%>%</span>\n  <span class = 'fun_call_code'>mutate</span><span class = 'bracket_code'>(</span>y = <span class = 'fun_call_code'>mean</span><span class = 'bracket_code'>(</span>my_vec<span class = 'bracket_code'>)</span><span class = 'bracket_code'>)</span> <span class = 'fun_call_code'>%>%</span>\n  <span class = 'fun_call_code'>select</span><span class = 'bracket_code'>(</span>y<span class = 'bracket_code'>)</span>",
                     "<span class = 'fun_call_code'>str</span><span class = 'bracket_code'>(</span>my_df<span class = 'bracket_code'>)</span>",
                     "my_df"))
})
