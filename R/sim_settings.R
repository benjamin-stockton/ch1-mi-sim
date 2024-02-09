#simulation set-up
library(dplyr)
source("R/create_setting_data_frame.R")

######
# Test
######

(test_setting <- create_setting_data_frame(N_sample = c(250),
                            N_sim = c(5),
                            name_DGP = c("PN Low Conc"),
                           p_miss = c(0.1),
                           beta = c(0, 0, 1, 0.5)))

saveRDS(test_setting, 'sim_settings/test-setting.rds')

################
# Basic Settings to vary DGP
################

(setting <- create_setting_data_frame(N_sample = c(500),
                                   N_sim = c(250),
                                   name_DGP = c("PN High Conc", "PN Low Conc", "PN Skewed", "PN Bi modal", "PN Reg", "vM Reg"),
                                   p_miss = c(0.5),
                                   beta = c(-0.55, 0, -0.2, 0.3)))
saveRDS(setting, "sim_settings/dgp-setting.rds")

#####
# PN Low Concentration
#####

(setting <- create_setting_data_frame(N_sample = c(100, 500, 1000),
                                     N_sim = c(250),
                                     DGP = "PN",
                                     name_DGP = "PN Low Conc",
                                     p_miss = c(0.1, 0.5, 0.9),
                                     beta = c(-0.55, 0, -0.2, 0.3)))

saveRDS(setting, 'sim_settings/pn-lc-setting.rds')

#####
# PN Regression
#####

(setting <- create_setting_data_frame(N_sample = c(100, 500, 1000),
                                     N_sim = c(250),
                                     DGP = "PNreg",
                                     name_DGP = "PN Reg",
                                     p_miss = c(0.1, 0.5, 0.9),
                                     beta = c(-0.55, 0, -0.2, 0.3)))

saveRDS(setting, 'sim_settings/pn-reg-setting.rds')
