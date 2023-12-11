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
# Data Generation
######################################

N <- 100; sigma <- 1; 
beta <- c(0,0,-7,6)
# beta <- c(10, 2, 0.5, 0)
# beta <- round(c(rnorm(2, 0, 5), rnorm(2, 0, 1)), 2)
beta0 <- beta
alpha <- c(0, 0, 0, 0, 0, 5)

M <- 100

mu_i <- c(1,0)
# sigma_mat <- c(1,-2, -2, 10)
sigma_mat <- c(1,0,0,1)
prop_miss <- 0.5

sim.dat <- generate_data(name_DGP = "PN", N = N, sigma = sigma, beta = beta, mu = mu_i,
                         sigma_mat = sigma_mat)
theta <- my_atan2(sim.dat$Xs, sim.dat$Xc)
inc.dat <- impose_missing(sim.dat, alpha = alpha, p_miss = prop_miss)
sim.dat.inc <- inc.dat$dat.inc; mis <- inc.dat$mis_ind
sim.dat.obs <- inc.dat$dat.obs; sim.dat.mis <- inc.dat$dat.mis
theta.obs <- my_atan2(sim.dat.obs$Xs, sim.dat.obs$Xc)
theta.mis <- my_atan2(sim.dat[mis, "Xs"], sim.dat[mis, "Xc"])

r_ind <- numeric(N)
r_ind[mis] <- 1
r_ind <- abs(1 - r_ind)

sim.dat.obs$theta <- my_atan2(sim.dat.obs$Xs, sim.dat.obs$Xc)
sim.dat.inc$theta <- my_atan2(sim.dat.inc$Xs, sim.dat.inc$Xc)

######################################
# Imputations
######################################

# Imputation on (X_c, X_s)
# Normal Regression
norm.cs <- multiply_impute(sim.dat.inc, impute_type = "norm", impute_on_theta = F, M = M, printFlag = F)

# PMM
pmm.cs <- multiply_impute(sim.dat.inc, impute_type = "pmm", impute_on_theta = F, M = M, printFlag = F)

# Imputation on theta
# Normal
norm.theta <- multiply_impute(sim.dat.inc, impute_type = "norm", impute_on_theta = T, M = M, printFlag = F)

# pmm
pmm.theta <- multiply_impute(sim.dat.inc, impute_type = "pmm", impute_on_theta = T, M = M, printFlag = F)

# bpnreg
bpn.theta <- multiply_impute(sim.dat.inc, impute_type = "bpnreg", impute_on_theta = T, M = M, printFlag = F)

calc_median_imp_error <- function(long.imps, theta_masked, M) {
    long.imps[which(long.imps$miss_pattern == "Imputed Angles"), "masked_theta"] <- rep(theta_masked, M + 1)
    
    inc.dat2 <- long.imps[which((long.imps$miss_pattern == "Imputed Angles") & long.imps$.imp > 0),]
    
    # inc.dat2$imp_dist <- pi - abs(pi - abs(inc.dat2$masked_theta - inc.dat2$theta))
    inc.dat2$imp_dist <- sqrt((inc.dat2$Xc - cos(inc.dat2$masked_theta))^2 + (inc.dat2$Xs - sin(inc.dat2$masked_theta))^2)
    
    return(median(inc.dat2$imp_dist))
}


inc.dat <- bind_rows(list(bpn.theta$long.imps, 
                          pmm.theta$long.imps, norm.theta$long.imps,
                          pmm.cs$long.imps, norm.cs$long.imps))

inc.dat$imp_method <- c(rep("bpnreg", 10100), rep("pmm_theta", 10100), rep("norm_theta", 10100),
                        rep("pmm_cs", 10100), rep("norm_cs", 10100))
dim(inc.dat[which((inc.dat$miss_pattern == "Imputed Angles")),])

inc.dat[which((inc.dat$miss_pattern == "Imputed Angles")),"masked_theta"] <- rep(theta.mis, M + 1)

inc.dat2 <- inc.dat[which((inc.dat$miss_pattern == "Imputed Angles") & inc.dat$.imp > 0),]

# inc.dat2$imp_dist <- pi - abs(pi - abs(inc.dat2$masked_theta - inc.dat2$theta))
inc.dat2$imp_dist <- sqrt((inc.dat2$Xc - cos(inc.dat2$masked_theta))^2 + (inc.dat2$Xs - sin(inc.dat2$masked_theta))^2)

imp.dat <- inc.dat2 %>% tidyr::pivot_wider(names_from = imp_method, values_from = c(Xc, Xs, theta, imp_dist))

p1 <- ggplot(inc.dat2, aes(Xc, Xs, color = imp_dist)) +
    geom_point() +
    facet_wrap(imp_method~.) +
    scale_color_viridis_c(option = "B")

p2 <- ggplot(inc.dat2, aes(imp_method, imp_dist, color = imp_dist)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
    geom_jitter(width = .05) +
    scale_color_viridis_c(option = "B")

cowplot::plot_grid(p1, p2, nrow = 2)

calc_median_imp_error(bpn.theta$long.imps, theta.mis, M = M)
apply(imp.dat[,paste0("imp_dist_", unique(inc.dat2$imp_method))], 2, median)
