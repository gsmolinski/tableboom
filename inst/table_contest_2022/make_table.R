dir_path <- system.file(package = "tableboom", "table_contest_2022")

path_to_inspect <- file.path(dir_path, "children_from_ukr_temp_prot_eu.R")
path_to_save <- NULL
# path_to_save <- file.path("~", "tableboom.html")

tableboom::inspect_r(path_to_inspect, path_to_save)
