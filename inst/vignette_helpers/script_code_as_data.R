# define data
my_df <- data.frame(col_a = c("one", "two"),
                    col_b = c(1, 2))

my_df$col_c <- my_df$col_b + 2 # add 2 to each row in col_b

my_df

sum(my_df$col_b); mean(my_df$col_c)
