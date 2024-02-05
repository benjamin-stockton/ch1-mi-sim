# Complete Case Analysis Simulation Script.R

##########################
# Load Packages
##########################

library(circular)
library(dplyr)
library(ggplot2)
library(mvtnorm)
library(doParallel)
# library(rgl)
# options(rgl.useNULL=TRUE)

# library(cmdstanr)
# check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
library(latex2exp)
color_scheme_set("brightblue")

# check_cmdstan_toolchain()

# install_cmdstan(cores = 5, overwrite = F)
# PATH_TO_CMDSTAN <- cmdstan_path()
# set_cmdstan_path(PATH_TO_CMDSTAN)
# cmdstan_version()
# 
# file <- file.path(getwd(), "Stan_Models/Models/proj_normal_identity_reg.stan")
# mod <- cmdstan_model(file)

theme_set(theme_bw())

file.sources = list.files(c("R/linear-reg-sim"), 
                          pattern="*.R$", full.names=TRUE, 
                          ignore.case=TRUE)
sapply(file.sources,source,.GlobalEnv)
source("R/analysis_helpers.R")

######################################
# Parse CL Arguments
######################################

args <- commandArgs(trailingOnly = T)

if (length(args) != 2) {
    stop("Simulation parameters misspecificed. Should be Q, 'DGP_name'")
} else if (length(args) == 2) {
    # Number of iterations to run in each parallel thread
    simQ <- as.numeric(args[1]) # Q
    # Sample size
    name_DGP <- args[2]
}
# sim.settings <- read.csv("Sim_Results/Sim_Settings.csv", header = T)
sim.settings <- read.csv("Sim_Results/Sim_Settings_big.csv", header = T)

######################################
# Data Generation
######################################

sim.settings <- sim.settings %>% filter(DGP_Name == name_DGP)
DGP_type <- sim.settings[1, "DGP"]

# print(sim.settings)

for (i in 1:nrow(sim.settings)) {
    N <- sim.settings[i, "N"]
    sigma <- sim.settings[i, "sigma"]
    beta <- as.numeric(sim.settings[i, paste0("beta_", 1:4)])
    beta0 <- beta
    alpha <- as.numeric(sim.settings[i, paste0("alpha_", 1:6)])

    M <- sim.settings[i, "M"]

    mu_i <- as.numeric(sim.settings[i, paste0("mu_", 1:2)])
    sigma_mat <- as.numeric(sim.settings[i, paste0("sigma_mat_", 1:4)])
    prop_miss <- sim.settings[i, "p_miss"]

    ########################################
    # Analysis Simulations
    ########################################
    print(paste0("Simulating with ", simQ, " iterations"))
    fp <- file.path("Sim_Results", name_DGP, paste0("p_miss_", prop_miss, "_N_", N),
                    paste0("mu_", stringr::str_c(mu_i, collapse = "_"),
                           "_sigma_mat_", stringr::str_c(sigma_mat, collapse = "_")))
    generic_output_path <- paste0("/OLS_sim_iters_", simQ, "_M_", M,
                                  "_beta_", stringr::str_c(beta, collapse = "_"),
                                  "_alpha_", stringr::str_c(alpha, collapse = "_"),
                                  "_CCA")
    print(paste0("Output path:", fp, generic_output_path))

    set.seed(98672)
    cores <- parallel::detectCores()
    coreQ <- ceiling(simQ / (cores[1] - 1))

    cl <- makeCluster(cores[1] - 1)
    registerDoParallel(cl)
    ptime <- system.time({
        res <- foreach(i = 1:(cores[1]-1), .combine = rbind) %dopar% {
            file.sources = list.files(c("R/linear-reg-sim"),
                                      pattern="*.R$", full.names=TRUE,
                                      ignore.case=TRUE)
            sapply(file.sources,source,.GlobalEnv)
            source("R/analysis_helpers.R")
            cca_reg_sim(name_DGP = DGP_type, simQ = coreQ, N = N, beta = beta,
                          sigma = sigma, Q0 = beta, p_miss = prop_miss,
                          mu_i = mu_i, sigma_mat = sigma_mat, alpha = alpha)
        }
    })[3]
    print(ptime)
    stopCluster(cl)

    #################
    res$DGP <- name_DGP
    res$N <- N
    res$M <- M
    res$sigma <- sigma
    res$mu_1 <- mu_i[1]
    res$mu_2 <- mu_i[2]
    res$sigma_mat_1 <- sigma_mat[1]
    res$sigma_mat_2 <- sigma_mat[2]
    res$sigma_mat_3 <- sigma_mat[3]
    res$sigma_mat_4 <- sigma_mat[4]
    res$alpha_1 <- alpha[1]
    res$alpha_2 <- alpha[2]
    res$alpha_3 <- alpha[3]
    res$alpha_4 <- alpha[4]
    res$alpha_5 <- alpha[5]
    res$alpha_6 <- alpha[6]

    # res$Analysis %<>% forcats::fct_relevel(c("Complete", "Norm CS", "PMM CS", "Norm theta", "PMM theta", "bPNReg"))


    write.csv(res, paste0(fp, generic_output_path, "_results", ".csv"), row.names = F)
}


