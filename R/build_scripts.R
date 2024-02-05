library(dplyr)
library(purrr)

#################################################
## Test: MAR with Incompleteness only on Angles ##
#################################################

setting <- readRDS("sim_settings/test-setting.rds")

# setting.l <- as.list(setting%>%filter(set_n!=1))

purrr::pwalk(.l = setting,
             .f = function(N_sample, N_sim, p_miss, M, set_n, init_seed, DGP, name_DGP, beta_1, beta_2, beta_3, beta_4){
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
                             set_n = set_n,
                             init_seed = init_seed)
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
                    setting = setting)

####################################################
# Create simulation scripts for each data generating process
####################################################

dgps <- c("pn-bi", "pn-hc", "pn-lc", "pn-reg", "pn-skew", "vm-reg", "wn-reg")


for (d in dgps) {
    setting <- readRDS(paste0("sim_settings/", d, "-setting.rds"))
    
    print(setting)
    
    purrr::pwalk(.l = setting,
                 .f = function(N_sample, N_sim, p_miss, M, set_n, DGP, reg_rel, name_DGP, beta_1, beta_2, beta_3, beta_4){
                     cat(
                         whisker::whisker.render(
                             readLines(paste0('tmpls/', d, '-mi-sim.tmpl')),
                             data = list(
                                 N_sample = N_sample,
                                 N_sim = N_sim,
                                 M = M,
                                 p_miss = p_miss,
                                 DGP = DGP,
                                 reg_rel = reg_rel,
                                 name_DGP = name_DGP,
                                 beta_1 = beta_1,
                                 beta_2 = beta_2,
                                 beta_3 = beta_3,
                                 beta_4 = beta_4,
                                 set_n = set_n)
                         ),
                         file = file.path('sim_scripts',
                                          sprintf(paste0(d, "-mi-sim_setting-%s.R"),
                                                  set_n)
                         ),
                         sep='\n')
                 })

    create_bash_scripts("sim_scripts",
                        script_base_name = paste0(d, "-mi-sim_setting-%s.R"),
                        sh_name = paste0("mar-", d, "-sim.sh"),
                        setting = setting)
}

