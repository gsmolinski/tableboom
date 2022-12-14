test_that("'set_options' gives the desired effect", {
    user_options <- get_user_options()
    suppressMessages(set_options())

    example_df <- data.frame(x = 1:10, y = 2:11)
    expected <- c("<  dplyr::select(dplyr::select(example_df, x, y), y) ", ". <  >  dplyr::select(example_df, x, y) ",
                  ". Rows: 10", ". Columns: 2", ". $ x <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10",
                  ". $ y <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 11", ". ", ">  dplyr::select(dplyr::select(example_df, x, y), y) ",
                  "Rows: 10", "Columns: 1", "$ y <int> 2, 3, 4, 5, 6, 7, 8, 9, 10, 11",
                  "", "    y", "1   2", "2   3", "3   4", "4   5", "5   6", "6   7",
                  "7   8", "8   9", "9  10", "10 11")
    obj <- example_df |>
      dplyr::select(x, y) |>
      dplyr::select(y) |>
      boomer::boom() |>
      utils::capture.output()

    expect_identical(obj, expected)
    expect_message(set_options())
    suppressMessages(restore_options(user_options))
})

test_that("'restore_options' gives the desired effect", {
  skip_if_not(interactive())
  user_options <- get_user_options()
  suppressMessages(set_options())
  suppressMessages(restore_options(user_options))

  example_df <- data.frame(x = 1:10, y = 2:11)
  expected <- c("馃挘 dplyr::select(dplyr::select(example_df, x, y), y) ",
                "路 馃挘 馃挜 dplyr::select(example_df, x, y) ", "路     x  y",
                "路 1   1  2", "路 2   2  3", "路 3   3  4", "路 4   4  5", "路 5   5  6",
                "路 6   6  7", "路 7   7  8", "路 8   8  9", "路 9   9 10", "路 10 10 11",
                "路 ", "馃挜 dplyr::select(dplyr::select(example_df, x, y), y) ",
                "    y", "1   2", "2   3", "3   4", "4   5", "5   6", "6   7",
                "7   8", "8   9", "9  10", "10 11", "", "    y", "1   2", "2   3",
                "3   4", "4   5", "5   6", "6   7", "7   8", "8   9", "9  10",
                "10 11")
  obj <- example_df |>
    dplyr::select(x, y) |>
    dplyr::select(y) |>
    boomer::boom() |>
    utils::capture.output()

  expect_identical(obj, expected)
  expect_message(restore_options(user_options))
})
