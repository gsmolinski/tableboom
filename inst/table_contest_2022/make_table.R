dir_path <- system.file(package = "tableboom", "table_contest_2022")

script_to_inspect <- file.path(dir_path, "children_from_ukr_temp_prot_eu.R")

inspect_r(script_to_inspect)
