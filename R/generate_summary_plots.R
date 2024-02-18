library(purrr)
library(dplyr)
library(ggplot2)
library(rsimsum)
source("R/create_res_summary.R")

in_dir <- "sim-results"
file_prefix <- "mar-pn-lc-mi-lm-sim_setting-"
file_suffix <- ".csv"
setting <- readRDS("sim_settings/pn-lc-setting.rds")
ll <- setting$set_n

res <- create_res_sum(ll = ll, in_dir = in_dir, file_prefix = file_prefix,
                      file_suffix = file_suffix)

lapply(res, function(betaj) {betaj$p_grid})



in_dir <- "sim-results"
file_prefix <- "mar-pn-reg-mi-lm-sim_setting-"
file_suffix <- ".csv"
setting <- readRDS("sim_settings/pn-reg-setting.rds")
ll <- setting$set_n

res <- create_res_sum(ll = ll, in_dir = in_dir, file_prefix = file_prefix,
                      file_suffix = file_suffix)

lapply(res, function(betaj) {betaj$p_grid})

in_dir <- "sim-results"
file_prefix <- "mar-dgp-mi-lm-sim_setting-"
file_suffix <- ".csv"
setting <- readRDS("sim_settings/dgp-setting.rds")
ll <- setting$set_n

res <- create_res_sum(ll = ll, in_dir = in_dir, file_prefix = file_prefix,
                      file_suffix = file_suffix, by = "name_DGP")

lapply(res, function(betaj) {betaj$p_grid})
