library(dplyr)
library(purrr)
library(tidyr)

setting <- readRDS("setting.rds")

################################
# All patient data is observed #
################################

source("R/create_res_summary.R")

ll <- 1:9

file_prefix <- "sim-results_2023-12-04"

create_res_sum(ll, load_rds = TRUE, file_prefix = file_prefix, file_suffix = ".csv")

create_ch_sum(ll, load_rds = FALSE, file_prefix = file_prefix)

create_minfo_sum(ll, load_rds = FALSE, file_prefix = file_prefix)

