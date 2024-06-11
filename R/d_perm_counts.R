# Load packages -----
source("R/_load_packages.R")
source("R/_db_connect.R")
source("R/_project_directory.R")

perm_locs_orig <- dbReadTable(db_connect(), name = c("automated","perm_locs"))
perm_locs <- perm_locs_orig

perm_locs <- 
  perm_locs %>% 
    mutate(obs_duration_cleaned = case_when(
    obs_duration == "4 years 11 mons 30 days 23:00:00" ~ "5 years 0 mons 00 days 00:00:00", 
    TRUE ~ obs_duration
  )) 

export_rdata(perm_locs, 'perm_locs.rds')

cli::cli_inform(
  c("v" = "Okay...your data has been pulled and prepped\n")
)