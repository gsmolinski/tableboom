library(dplyr)

dir <- system.file(package = "tableboom", "example_script")

source(paste0(dir, "/example_script_sourced_inside.R"))

my_fun2 <- function(x) {
  x + 2
}

my_fun3 <<- function(x) {
  x + 2
}

function(x) {
  x + 2
} -> my_fun4

assign("my_fun5", function(x) x + 2)

for (i in 1:3) {
  cat(i)
}

my_df["x"]

my_df

my_vec <- rep(my_df$x + 10, 2)

my_df <- my_df %>%
  mutate(y = mean(my_vec)) %>%
  select(y)

str(my_df)
my_df
