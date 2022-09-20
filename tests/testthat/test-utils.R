user_options <- get_user_options()

test_that("'set_options' gives the desired effect", {
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

    suppressMessages(restore_options(user_options))
})

test_that("'restore_options' gives the desired effect", {
  skip_if_not(interactive())
  suppressMessages(set_options())
  suppressMessages(restore_options(user_options))

  example_df <- data.frame(x = 1:10, y = 2:11)
  expected <- c("💣 dplyr::select(dplyr::select(example_df, x, y), y) ",
                "· 💣 💥 dplyr::select(example_df, x, y) ", "·     x  y",
                "· 1   1  2", "· 2   2  3", "· 3   3  4", "· 4   4  5", "· 5   5  6",
                "· 6   6  7", "· 7   7  8", "· 8   8  9", "· 9   9 10", "· 10 10 11",
                "· ", "💥 dplyr::select(dplyr::select(example_df, x, y), y) ",
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
})
