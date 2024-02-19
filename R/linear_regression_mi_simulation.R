# linear_regression_mi_simulation.R 

calc_median_imp_error <- function(long.imps, theta_masked, M) {
    long.imps[which(long.imps$miss_pattern == "Imputed Angles"), "masked_theta"] <- rep(theta_masked, M + 1)
    
    inc.dat2 <- long.imps[which((long.imps$miss_pattern == "Imputed Angles") & long.imps$.imp > 0),]
    
    # inc.dat2$imp_dist <- pi - abs(pi - abs(inc.dat2$masked_theta - inc.dat2$theta))
    inc.dat2$imp_dist <- sqrt((inc.dat2$Xc - cos(inc.dat2$masked_theta))^2 + (inc.dat2$Xs - sin(inc.dat2$masked_theta))^2)
    
    return(median(inc.dat2$imp_dist))
}

pn_mi_reg_sim <- function(name_DGP, simQ = 10, N = 100, beta = c(.5, 1, .25, -.25),
                          sigma = 1, Q0 = c(3, .5, 1, .25, -.25), M = 15, p_miss = 0.25,
                          mu_i = c(10,0), sigma_mat = c(1,0,0,1),
                          alpha = c(.5, -2, 0, 0, 0, 0),
                          bPN_impute = TRUE, vM_impute = TRUE) {
    # Number of different analysis types
    A <- (6 + ifelse(bPN_impute, 3, 0) + ifelse(vM_impute, 2, 0))
    
    res <- data.frame("DGP" = character(0),
                      "Analysis" = character(0),
                      "beta_hat_0" = numeric(0),
                      "beta_hat_1" = numeric(0),
                      "beta_hat_2" = numeric(0),
                      "beta_hat_3" = numeric(0),
                      "beta_hat_4" = numeric(0),
                      "MoE_0" = numeric(0),
                      "MoE_1" = numeric(0),
                      "MoE_2" = numeric(0),
                      "MoE_3" = numeric(0),
                      "MoE_4" = numeric(0),
                      "prop_miss" = numeric(0),
                      "pval" = numeric(0),
                      "F_stat" = numeric(0),
                      "MMSE" = numeric(0),
                      "beta_0" = numeric(0),
                      "beta_1" = numeric(0),
                      "beta_2" = numeric(0),
                      "beta_3" = numeric(0),
                      "beta_4" = numeric(0))
    
    for (q in 1:simQ) {
        
        # Simulate Data
        sim.dat <- generate_data(name_DGP = name_DGP, N = N, sigma = sigma, beta = beta,
                                 mu = mu_i, sigma_mat = sigma_mat)
        
        # Create Missing Values
        inc.dat <- impose_missing(sim.dat, alpha = alpha, p_miss = p_miss)
        sim.dat.inc <- inc.dat$dat.inc
        mis <- inc.dat$mis_ind
        
        if (length(mis) == 0) {
            print("No Missing Data!!")
            break
        }
        
        res_list <- list()
        median_imp_error <- numeric(A)
        
        # Complete Data analysis
        comp_ind <- A * (q - 1) + 1
        X <- as.matrix(sim.dat[,c("X1", "X2", "Xc", "Xs")])
        y <- sim.dat$Y
        res_list[["Comp"]] <- lm_complete(X = X, y = y, Q0 = Q0)
        
        # CCA
        X <- as.matrix(sim.dat.inc[,c("X1", "X2", "Xc", "Xs")])
        y <- sim.dat.inc$Y

        # print(length(mis))
        if (length(mis) > 0) {
            X <- X[-mis,]
            y <- y[-mis]
        }
        res_list[["CCA"]] <- lm_complete(X = X, y = y, Q0 = Q0)
        
        # Norm CS MI
        mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "norm", impute_on_angle = F)
        median_imp_error[3] <- calc_median_imp_error(mult_imps$long.imps,
                                    theta_masked = sim.dat[mis, "theta"], M = M)
        res_list[["norm_cs"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
        
        # PMM CS MI
        mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "pmm", impute_on_angle = F)
        median_imp_error[4] <- calc_median_imp_error(mult_imps$long.imps,
                                                    theta_masked = sim.dat[mis, "theta"], M = M)
        res_list[["pmm_cs"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
        
        # Norm theta MI
        mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "norm", impute_on_angle = T)
        median_imp_error[5] <- calc_median_imp_error(mult_imps$long.imps,
                                                    theta_masked = sim.dat[mis, "theta"], M = M)
        res_list[["norm_theta"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
        
        # PMM theta MI
        mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "pmm", impute_on_angle = T)
        median_imp_error[6] <- calc_median_imp_error(mult_imps$long.imps,
                                                    theta_masked = sim.dat[mis, "theta"], M = M)
        res_list[["pmm_theta"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
        
        
        if (bPN_impute & !vM_impute) {
            # bPN MI
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "bpnreg", impute_on_angle = T)
            median_imp_error[7] <- calc_median_imp_error(mult_imps$long.imps,
                                                        theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["bpnreg"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # pnregid MI
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "pnregid", impute_on_angle = T)
            median_imp_error[8] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["pnregid"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # pnregid MI
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "pnreggen", impute_on_angle = T)
            median_imp_error[9] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["pnreggen"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            Analysis <- c("Complete", "CCA", "Norm CS", "PMM CS", "Norm theta", "PMM theta", "bPNReg", "pnregid", "pnreggen")
        }
        else if (vM_impute & !bPN_impute) {
            # vM Reg with Intercept outside link function
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "vmreg", impute_on_angle = T)
            median_imp_error[7] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["vmreg"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # vM Reg with Intercept in link function
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "vmbrms", impute_on_angle = T)
            median_imp_error[8] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["vmbrms"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            Analysis <- c("Complete", "CCA", "Norm CS", "PMM CS", "Norm theta", "PMM theta", "vMreg", "vmbrms")
        }
        else if (vM_impute & bPN_impute) {
            # bPN MI
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "bpnreg", impute_on_angle = T)
            median_imp_error[7] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["bpnreg"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # pnregid MI
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "pnregid", impute_on_angle = T)
            median_imp_error[8] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["pnregid"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # pnregid MI
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "pnreggen", impute_on_angle = T)
            median_imp_error[9] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["pnreggen"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # vM Reg with Intercept outside link function
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "vmreg", impute_on_angle = T)
            median_imp_error[10] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["vmreg"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            # vM Reg with Intercept in link function
            mult_imps <- multiply_impute(sim.dat.inc, M = M, impute_type = "vmbrms", impute_on_angle = T)
            median_imp_error[11] <- calc_median_imp_error(mult_imps$long.imps,
                                                         theta_masked = sim.dat[mis, "theta"], M = M)
            res_list[["vmbrms"]] <- lm_combine(mult_imps = mult_imps$long.imps, M = M, Q0 = Q0)
            
            Analysis <- c("Complete", "CCA", "Norm CS", "PMM CS", "Norm theta", "PMM theta", "bPNReg", "pnregid", "pnreggen", "vMreg", "vmbrms")
        }
        else {
            Analysis <- c("Complete", "CCA", "Norm CS", "PMM CS", "Norm theta", "PMM theta")
        }
        beta_hats_moe <- matrix(NA, nrow = A, ncol = 5*2)
        pval <- numeric(A)
        F_stat <- numeric(A)
        for (i in 1:A) {
            beta_hats_moe[i,] <- c(res_list[[i]]$Qbar, res_list[[i]]$MoE)
            pval[i] <- res_list[[i]]$pval
            F_stat[i] <- res_list[[i]]$Fstat
        }
        
        iter_res <- data.frame(
            "DGP" = name_DGP,
            "Analysis" = Analysis,
            "beta_hat_0" = beta_hats_moe[,1],
            "beta_hat_1" = beta_hats_moe[,2],
            "beta_hat_2" = beta_hats_moe[,3],
            "beta_hat_3" = beta_hats_moe[,4],
            "beta_hat_4" = beta_hats_moe[,5],
            "MoE_0" = beta_hats_moe[,6],
            "MoE_1" = beta_hats_moe[,7],
            "MoE_2" = beta_hats_moe[,8],
            "MoE_3" = beta_hats_moe[,9],
            "MoE_4" = beta_hats_moe[,10],
            "median_imp_error" = median_imp_error,
            "prop_miss" = length(mis) / N,
            "pval" = pval,
            "F_stat" = F_stat,
            "beta_0" = 3,
            "beta_1" = beta[1],
            "beta_2" = beta[2],
            "beta_3" = beta[3],
            "beta_4" = beta[4]
        )
        
        res <- rbind(res, iter_res)
    }
    
    # res <- cbind(res, resid_y)
    return(res)
}


