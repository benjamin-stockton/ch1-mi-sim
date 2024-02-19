# mice_impute_circglm.R

mice.impute.circglm <- function(y, ry, x,...) {
    
    dat <- as.data.frame(cbind(y[ry], x[ry,]))
    dat$theta <- as.numeric(circular::minusPiPlusPi(circular(dat[,1])))
    dat <- dat[,-1]
    
    fit <- circglmbayes::circGLM(theta ~ ., data = dat, 
                                 Q = 100, thin = 10, burnin = 25000,
                                 bt_prior_musd = c(mu = 0, sd = 5),
                                 r = 2)
    
    all_chains <- fit$all_chains
    
    x_tilde <- x[!ry,]
    s <- sample(nrow(all_chains), size = 1)
    beta_pd <- all_chains[s, 3:(ncol(x)+2)]
    theta_imp <- numeric(nrow(x_tilde))
    for (i in 1:nrow(x_tilde)) {
        eta_i <- x_tilde[i,] %*% beta_pd
        mu_i <- circular::circular(all_chains[s, "b0_chain"] + 2 * atan(eta_i))
        theta_imp[i] <- circular::rvonmises(1, mu_i, all_chains[s, "kp_chain"])
    }
    theta_imp <- as.numeric(theta_imp)
    
    return(theta_imp)
}


# y <- as.numeric(dat.mice$theta)
# x <- as.matrix(dat.mice[,c("X", "Y")])
# ry <- !is.na(y)
# 
# 
# dat <- as.data.frame(cbind(y[ry], x[ry,]))
# dat$theta <- as.numeric(circular::minusPiPlusPi(circular(dat[,1])))
# dat <- dat[,-1]
# 
# fit <- circglmbayes::circGLM(theta ~ ., data = dat, 
#                              Q = 2000, thin = 50, burnin = 50000,
#                              bt_prior_musd = c(mu = 0, sd = 5),
#                              r = 2)
# 
# all_chains <- fit$all_chains
# 
# x_tilde <- x[!ry,]
# s <- sample(nrow(all_chains), size = 1)
# beta_pd <- all_chains[s, 3:(ncol(x)+2)]
# theta_imp <- numeric(nrow(x_tilde))
# for (i in 1:nrow(x_tilde)) {
#     eta_i <- t(x_tilde[i,]) %*% beta_pd
#     mu_i <- circular::circular(all_chains[s, "b0_chain"] + 2 * atan(eta_i))
#     theta_imp[i] <- circular::rvonmises(1, mu_i, all_chains[s, "kp_chain"])
# }
