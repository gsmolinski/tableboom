library(dplyr)

path <- file.path(system.file(package = "tableboom","table_contest_2022", "inner_script"),
                          "inner_script.R")

source(path) # get data - children (below 18) from Ukraine which found temporary protection in EU countries

names(child_ukr_prot) <- c("country", "march_2022", "april_2022",
                           "may_2022","june_2022", "july_2022", "august_2022")

child_ukr_prot[5, "country"] <- stringi::stri_replace_all_regex(child_ukr_prot[5, "country"],
                                                                "\\s+.+", "")

child_ukr_prot <- child_ukr_prot |>
  mutate(across(matches("\\d$"), \(e) if_else(e == ":", NA_character_, e)),
         across(matches("\\d$"), as.integer))


