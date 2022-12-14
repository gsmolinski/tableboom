library(dplyr)

path <- file.path(system.file(package = "tableboom","table_contest_2022", "inner_script"),
                  "eurostat_data.R")
# Unfortunately, comments in separate lines are not included
source(path) # data - children (< 18) from Ukraine which found temp protection

names(child_ukr_prot) <- c("country", "march_2022", "april_2022",
                           "may_2022","june_2022", "july_2022", "august_2022")

child_ukr_prot[5, "country"] <- stringi::stri_replace_all_regex(child_ukr_prot[5, "country"],
                                                                "\\s+.+", "")

child_ukr_prot <- child_ukr_prot |>
  mutate(across(matches("\\d$"), \(e) if_else(e == ":", NA_character_, e)),
         across(matches("\\d$"), as.integer))

maxs <- vector("integer", ncol(child_ukr_prot) - 1)

for (i in 2:(ncol(child_ukr_prot))) {
  maxs[[i - 1]] <- max(child_ukr_prot[[i]], na.rm = TRUE)
}

maxs

total_months <- colSums(child_ukr_prot[, 2:ncol(child_ukr_prot)], na.rm = TRUE)

format(sum(total_months), big.mark = " ")

path()
