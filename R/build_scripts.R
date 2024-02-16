library(dplyr)
library(purrr)

#################################################
## Test: MAR with Incompleteness only on Angles ##
#################################################

setting <- readRDS("sim_settings/test-setting.rds")

# setting.l <- as.list(setting%>%filter(set_n!=1))

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP,
                           beta_1, beta_2, beta_3, beta_4, mu_1, mu_2,
                           Sigma_11, Sigma_12, Sigma_21, Sigma_22){
                 cat(
                     whisker::whisker.render(
                         readLines('tmpls/mar-mi-lm-sim.tmpl'),
                         data = list(
                             N_sample = N_sample,
                             N_sim = N_sim,
                             M = M,
                             p_miss = p_miss,
                             DGP = DGP,
                             name_DGP = name_DGP,
                             beta_1 = beta_1,
                             beta_2 = beta_2,
                             beta_3 = beta_3,
                             beta_4 = beta_4,
                             mu_1 = mu_1,
                             mu_2 = mu_2,
                             Sigma_11 = Sigma_11,
                             Sigma_12 = Sigma_12,
                             Sigma_21 = Sigma_21,
                             Sigma_22 = Sigma_22,
                             set_n = set_n,
                             init_seed = init_seed,
                             cores = 5,
                             out_prefix = "test-")
                     ),
                     file = file.path('sim_scripts',
                                      sprintf("test-mar-mi-lm-sim-%s.R",
                                              set_n)
                     ),
                     sep='\n')
             })

source("R/create_bash_scripts.R")

create_bash_scripts("sim_scripts",
                    script_base_name = "test-mar-mi-lm-sim-%s.R",
                    sh_name = "test-mar-mi-lm-sim.sh",
                    setting = setting,
                    cores = 5)

####################################################
# Create simulation scripts for each data generating process
####################################################

setting <- readRDS(paste0("sim_settings/dgp-setting.rds"))
    
print(setting)

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP,
                           beta_1, beta_2, beta_3, beta_4, mu_1, mu_2,
                           Sigma_11, Sigma_12, Sigma_21, Sigma_22){
                 cat(
                     whisker::whisker.render(
                         readLines(paste0('tmpls/mar-mi-lm-sim.tmpl')),
                         data = list(
                             N_sample = N_sample,
                             N_sim = N_sim,
                             M = M,
                             p_miss = p_miss,
                             DGP = DGP,
                             name_DGP = name_DGP,
                             beta_1 = beta_1,
                             beta_2 = beta_2,
                             beta_3 = beta_3,
                             beta_4 = beta_4,
                             mu_1 = mu_1,
                             mu_2 = mu_2,
                             Sigma_11 = Sigma_11,
                             Sigma_12 = Sigma_12,
                             Sigma_21 = Sigma_21,
                             Sigma_22 = Sigma_22,
                             set_n = set_n,
                             init_seed = init_seed,
                             cores = 125,
                             out_prefix = "mar-dgp-")
                     ),
                     file = file.path('sim_scripts',
                                      sprintf(paste0("mar-dgp-mi-sim_setting-%s.R"),
                                              set_n)
                     ),
                     sep='\n')
             })

create_bash_scripts("sim_scripts",
                    script_base_name = paste0("mar-dgp-mi-sim_setting-%s.R"),
                    sh_name = paste0("mar-dgp-mi-lm-sim.sh"),
                    setting = setting,
                    cores = 125)

####################################################
# Create simulation scripts for PN LC DGP
####################################################

setting <- readRDS(paste0("sim_settings/pn-lc-setting.rds"))

print(setting)

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP,
                           beta_1, beta_2, beta_3, beta_4, mu_1, mu_2,
                           Sigma_11, Sigma_12, Sigma_21, Sigma_22){
                 cat(
                     whisker::whisker.render(
                         readLines(paste0('tmpls/mar-mi-lm-sim.tmpl')),
                         data = list(
                             N_sample = N_sample,
                             N_sim = N_sim,
                             M = M,
                             p_miss = p_miss,
                             DGP = DGP,
                             name_DGP = name_DGP,
                             beta_1 = beta_1,
                             beta_2 = beta_2,
                             beta_3 = beta_3,
                             beta_4 = beta_4,
                             mu_1 = mu_1,
                             mu_2 = mu_2,
                             Sigma_11 = Sigma_11,
                             Sigma_12 = Sigma_12,
                             Sigma_21 = Sigma_21,
                             Sigma_22 = Sigma_22,
                             set_n = set_n,
                             init_seed = init_seed,
                             cores = 125,
                             out_prefix = "mar-pn-lc-")
                     ),
                     file = file.path('sim_scripts',
                                      sprintf(paste0("mar-pn-lc-mi-sim_setting-%s.R"),
                                              set_n)
                     ),
                     sep='\n')
             })

create_bash_scripts("sim_scripts",
                    script_base_name = paste0("mar-pn-lc-mi-sim_setting-%s.R"),
                    sh_name = paste0("mar-pn-lc-mi-lm-sim.sh"),
                    setting = setting,
                    cores = 125)

####################################################
# Create simulation scripts for PN Reg DGP
####################################################

setting <- readRDS(paste0("sim_settings/pn-reg-setting.rds"))

print(setting)

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP,
                           beta_1, beta_2, beta_3, beta_4, mu_1, mu_2,
                           Sigma_11, Sigma_12, Sigma_21, Sigma_22){
                 cat(
                     whisker::whisker.render(
                         readLines(paste0('tmpls/mar-mi-lm-sim.tmpl')),
                         data = list(
                             N_sample = N_sample,
                             N_sim = N_sim,
                             M = M,
                             p_miss = p_miss,
                             DGP = DGP,
                             name_DGP = name_DGP,
                             beta_1 = beta_1,
                             beta_2 = beta_2,
                             beta_3 = beta_3,
                             beta_4 = beta_4,
                             mu_1 = mu_1,
                             mu_2 = mu_2,
                             Sigma_11 = Sigma_11,
                             Sigma_12 = Sigma_12,
                             Sigma_21 = Sigma_21,
                             Sigma_22 = Sigma_22,
                             set_n = set_n,
                             init_seed = init_seed,
                             cores = 125,
                             out_prefix = "mar-pn-reg-")
                     ),
                     file = file.path('sim_scripts',
                                      sprintf(paste0("mar-pn-reg-mi-sim_setting-%s.R"),
                                              set_n)
                     ),
                     sep='\n')
             })

create_bash_scripts("sim_scripts",
                    script_base_name = paste0("mar-pn-reg-mi-sim_setting-%s.R"),
                    sh_name = paste0("mar-pn-reg-mi-lm-sim.sh"),
                    setting = setting,
                    cores = 125)

####################################################
# Create simulation scripts for PN LC DGP
####################################################

setting <- readRDS(paste0("sim_settings/pn-lc-setting.rds"))

print(setting)

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP,
                           beta_1, beta_2, beta_3, beta_4, mu_1, mu_2,
                           Sigma_11, Sigma_12, Sigma_21, Sigma_22){
                 cat(
                     whisker::whisker.render(
                         readLines(paste0('tmpls/mar-mi-lm-sim-cca.tmpl')),
                         data = list(
                             N_sample = N_sample,
                             N_sim = N_sim,
                             M = M,
                             p_miss = p_miss,
                             DGP = DGP,
                             name_DGP = name_DGP,
                             beta_1 = beta_1,
                             beta_2 = beta_2,
                             beta_3 = beta_3,
                             beta_4 = beta_4,
                             mu_1 = mu_1,
                             mu_2 = mu_2,
                             Sigma_11 = Sigma_11,
                             Sigma_12 = Sigma_12,
                             Sigma_21 = Sigma_21,
                             Sigma_22 = Sigma_22,
                             set_n = set_n,
                             init_seed = init_seed,
                             cores = 25,
                             out_prefix = "mar-pn-lc-")
                     ),
                     file = file.path('sim_scripts',
                                      sprintf(paste0("mar-pn-lc-mi-sim_setting-%s-cca.R"),
                                              set_n)
                     ),
                     sep='\n')
             })

create_bash_scripts("sim_scripts",
                    script_base_name = paste0("mar-pn-lc-mi-sim_setting-%s-cca.R"),
                    sh_name = paste0("mar-pn-lc-mi-lm-sim-cca.sh"),
                    setting = setting,
                    cores = 25)

####################################################
# Create simulation scripts for PN Reg DGP
####################################################

setting <- readRDS(paste0("sim_settings/pn-reg-setting.rds"))

print(setting)

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP,
                           beta_1, beta_2, beta_3, beta_4, mu_1, mu_2,
                           Sigma_11, Sigma_12, Sigma_21, Sigma_22){
                 cat(
                     whisker::whisker.render(
                         readLines(paste0('tmpls/mar-mi-lm-sim-cca.tmpl')),
                         data = list(
                             N_sample = N_sample,
                             N_sim = N_sim,
                             M = M,
                             p_miss = p_miss,
                             DGP = DGP,
                             name_DGP = name_DGP,
                             beta_1 = beta_1,
                             beta_2 = beta_2,
                             beta_3 = beta_3,
                             beta_4 = beta_4,
                             mu_1 = mu_1,
                             mu_2 = mu_2,
                             Sigma_11 = Sigma_11,
                             Sigma_12 = Sigma_12,
                             Sigma_21 = Sigma_21,
                             Sigma_22 = Sigma_22,
                             set_n = set_n,
                             init_seed = init_seed,
                             cores = 25,
                             out_prefix = "mar-pn-reg-")
                     ),
                     file = file.path('sim_scripts',
                                      sprintf(paste0("mar-pn-reg-mi-sim_setting-%s-cca.R"),
                                              set_n)
                     ),
                     sep='\n')
             })

create_bash_scripts("sim_scripts",
                    script_base_name = paste0("mar-pn-reg-mi-sim_setting-%s-cca.R"),
                    sh_name = paste0("mar-pn-reg-mi-lm-sim-cca.sh"),
                    setting = setting,
                    cores = 25)

