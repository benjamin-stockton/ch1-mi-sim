# impute_by_bPN_lin_reg.R

impute_by_bPN <- function(dat.obs, dat.mis, filepath = "Stan_Models/Models/proj_normal_identity.stan") {
    n_miss <- nrow(dat.mis)
    U <- as.matrix(dat.obs[, c("Xc", "Xs")])
    X <- as.matrix(dat.obs[,c("Y", "X1", "X2")])
    X <- cbind(rep(1, nrow(X)), X)
    Xtilde <- as.matrix(dat.mis[,c("Y", "X1", "X2")])
    Xtilde <- cbind(rep(1, nrow(Xtilde)), Xtilde)

    # Model with Int, Y, X1, X2
    data_list <- list(N = nrow(X), N_tilde = nrow(Xtilde), U = U, X = X, X_tilde = Xtilde, K = ncol(U), P = ncol(X))

    mod <- cmdstanr::cmdstan_model(filepath)

    fit1 <- mod$sample(
        data = data_list,
        chains = 4,
        parallel_chains = 4,
        refresh = 500 # print update every 500 iters
    )

    U_ppd <- posterior::as_draws_df(fit1$draws(variables = c("U_tilde"))) %>% as.matrix()
    theta_ppd <- matrix(NA, ncol = n_miss, nrow = nrow(U_ppd))
    for (i in 1:nrow(U_ppd)) {
        for (j in 1:n_miss) {
            theta_ppd[i,j] <- atan2(U_ppd[i, j+n_miss], U_ppd[i, j])
        }
    }

    U_tilde <- U_ppd[nrow(U_ppd),]
    dat.imp <- dat.mis
    dat.imp$Xc <- U_tilde[1:n_miss]
    dat.imp$Xs <- U_tilde[(n_miss+1):(2*n_miss)]
    dat.imp$theta <- my_atan2(dat.imp$Xs, dat.imp$Xc)

    if (!("theta" %in% colnames(dat.obs))) {
        dat.obs$theta <- my_atan2(dat.obs$Xs, dat.obs$Xc)
    }

    # Completed Data
    dat.comp.pn <- rbind(dat.obs, dat.imp)
    dat.comp.pn$miss_pattern <- c(rep("Complete", nrow(dat.obs)), rep("Imputed Angle", nrow(dat.imp)))

    return(list("completed" = dat.comp.pn, "ppds" = U_ppd))
}

