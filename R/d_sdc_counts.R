# Load packages -----
source("R/_load_packages.R")
source("R/_db_connect.R")
source("R/_project_directory.R")

sdc_locs_orig <- dbReadTable(db_connect(), name = c("automated","sdc_locs"))
sdc_locs <- sdc_locs_orig

export_rdata(sdc_locs, 'sdc_locs.rds')

sdc_counts_orig <- dbReadTable(db_connect(), name = c("received","nbpd_counts"))
sdc_counts <- sdc_counts_orig

sdc_counts <- 
  sdc_counts %>% filter(study_year != '2017')

export_rdata(sdc_counts, 'sdc_counts.rds')

cli::cli_inform(
  c("v" = "Okay...your data has been pulled and prepped\n")
)