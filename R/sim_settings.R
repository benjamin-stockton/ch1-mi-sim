#simulation set-up
library(dplyr)
source("R/create_setting_data_frame.R")

######
# Test
######

test_setting <- create_setting_data_frame(N_sample = c(100),
                            N_sim = c(5),
                            name_DGP = "PN High Conc",
                            DGP = "PN",
                           p_miss = c(0.1),
                           beta = c(c(0, 0, 1, 0.5), 
                                    c(10, 2, 0.5, 0)))

saveRDS(test_setting, 'sim_settings/test-setting.rds')

#####
# PN Bi-modal
#####

setting <- create_setting_data_frame(N_sample = c(100),
                       N_sim = c(250),
                       DGP = "PN",
                       name_DGP = "PN Bi modal",
                       p_miss = c(0.5),
                       beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/pn-bi-setting.rds')

#####
# PN High Concentration
#####

setting <- create_setting_data_frame(N_sample = c(100),
                                     N_sim = c(250),
                                     DGP = "PN",
                                     name_DGP = "PN High Conc",
                                     p_miss = c(0.5),
                                     beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/pn-hc-setting.rds')

#####
# PN Low Concentration
#####

setting <- create_setting_data_frame(N_sample = c(50, 100, 500, 1000),
                                     N_sim = c(250),
                                     DGP = "PN",
                                     name_DGP = "PN Low Conc",
                                     p_miss = c(0.1, 0.5, 0.9),
                                     beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/pn-lc-setting.rds')

#####
# PN Skewed
#####

setting <- create_setting_data_frame(N_sample = c(100),
                                     N_sim = c(250),
                                     DGP = "PN",
                                     name_DGP = "PN Skewed",
                                     p_miss = c(0.5),
                                     beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/pn-skew-setting.rds')

#####
# PN Reg
#####

setting <- create_setting_data_frame(N_sample = c(100),
                                     N_sim = c(250),
                                     DGP = "PNreg",
                                     name_DGP = "PN Reg",
                                     p_miss = c(0.5),
                                     beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/pn-reg-setting.rds')

#####
# vM Reg
#####

setting <- create_setting_data_frame(N_sample = c(100),
                                     N_sim = c(250),
                                     DGP = "wMreg",
                                     name_DGP = "vM Reg",
                                     p_miss = c(0.5),
                                     beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/vm-reg-setting.rds')

#####
# WN Reg
#####

setting <- create_setting_data_frame(N_sample = c(100),
                                     N_sim = c(250),
                                     DGP = "WNreg",
                                     name_DGP = "WN Reg",
                                     p_miss = c(0.5),
                                     beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0)))

saveRDS(setting, 'sim_settings/wn-reg-setting.rds')
