# linear_regression_cca_simulation.R 

calc_median_imp_error <- function(long.imps, theta_masked, M) {
    long.imps[which(long.imps$miss_pattern == "Imputed Angles"), "masked_theta"] <- rep(theta_masked, M + 1)
    
    inc.dat2 <- long.imps[which((long.imps$miss_pattern == "Imputed Angles") & long.imps$.imp > 0),]
    
    # inc.dat2$imp_dist <- pi - abs(pi - abs(inc.dat2$masked_theta - inc.dat2$theta))
    inc.dat2$imp_dist <- sqrt((inc.dat2$Xc - cos(inc.dat2$masked_theta))^2 + (inc.dat2$Xs - sin(inc.dat2$masked_theta))^2)
    
    return(median(inc.dat2$imp_dist))
}

cca_reg_sim <- function(name_DGP, simQ = 10, N = 100, beta = c(.5, 1, .25, -.25),
                          sigma = 1, Q0 = c(.5, 1, .25, -.25), p_miss = 0.25,
                          mu_i = c(10,0), sigma_mat = c(1,0,0,1), alpha = c(.5, -2, 0, 0, 0, 0)) {
    res <- data.frame("DGP" = character(0),
                      "Analysis" = character(0),
                      "beta_hat_0" = nummeric(0),
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
    
    A <- 2
    
    for (q in 1:simQ) {
        
        # Simulate Data
        sim.dat <- generate_data(name_DGP = name_DGP, N = N, sigma = sigma, beta = beta,
                                 mu = mu_i, sigma_mat = sigma_mat)
        
        # Create Missing Values
        inc.dat <- impose_missing(sim.dat, alpha = alpha, p_miss = p_miss)
        sim.dat.inc <- inc.dat$dat.inc; mis <- inc.dat$mis_ind
        
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
        # resid_y[comp_ind,] <- res_list[["Comp"]]$resid_y
        
        # CCA
        X <- as.matrix(sim.dat.inc[,c("X1", "X2", "Xc", "Xs")])
        y <- sim.dat.inc$Y
        res_list[["CCA"]] <- lm_complete(X = X, y = y, Q0 = Q0)
        
        Analysis <- c("Complete", "CCA")
        
        beta_hats_moe <- matrix(NA, nrow = A, ncol = 5*2)
        pval <- numeric(A)
        F_stat <- numeric(A)
        for (i in 1:A) {
            # y_pred <- cbind(rep(1, nrow(X)), X) %*% res_list[[i]]$Qbar
            # resid_y[comp_ind + i,] <- y - y_pred
            
            beta_hats_moe[i,] <- c(res_list[[i]]$Qbar[1:5], res_list[[i]]$MoE[1:5])
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


