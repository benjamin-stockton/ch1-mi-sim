# linear_regression_comp_and_combine.R 

lm_complete <- function(X, y, Q0 = rep(0, 5)) {
    k <- length(Q0)-1
    
    data <- cbind(X, y) %>% as.data.frame()
    
    fit.comp <- lm(y ~ X1 + X2 + Xc + Xs, data = data)
    
    beta.hat <- fit.comp$coefficients
    sigma.hat <- summary(fit.comp)$sigma^2
    se_beta <- summary(fit.comp)$coefficients[1:5,2]
    MoE <- qt(0.975, df = nrow(data) - k - 1) * se_beta
    
    Qdiff <- (beta.hat - Q0)
    X <- cbind(rep(1, nrow(X)), X)
    T_var <- (k * sigma.hat * solve(t(X) %*% X))
    test_stat <- colSums(Qdiff *  ((t(X) %*% X) %*% Qdiff)) / (k * sigma.hat)
    
    pval <- pf(test_stat, k, nrow(data) - k - 1, lower.tail = F)
    
    return(list(Qbar = beta.hat, sigma = sigma.hat, Tvar = T_var, Fstat = test_stat, pval = pval, df = c("df1" = k, "df2" = nrow(data) - k - 1), se.beta = se_beta, MoE = MoE, resid_y = residuals(fit.comp)))
}

lm_combine <- function(mult_imps, M = 3, Q0 = rep(0, 5)) {
    k <- length(Q0)-1
    
    Q <- matrix(data = NA, nrow = M, ncol = k+1)
    U <- matrix(data = rep(0, (k+1)^2), nrow = k+1, ncol = k+1)
    for (m in 1:M) {
        dat.m <- mult_imps[mult_imps$.imp == m, c("X1", "X2", "Xc", "Xs", "Y")]
        fit <- lm(Y ~ X1 + X2 + Xc + Xs, data = dat.m)
        X <- as.matrix(cbind(rep(1, nrow(dat.m)), dat.m[,c("X1", "X2", "Xc", "Xs")]))
        Q[m,] <- coefficients(fit)
        XtX <- t(X) %*% X
        if (!is.na(abs(det(XtX)))) {
            if (abs(det(XtX)) > 1e-15) {
                XtX_inv <- solve(XtX)
            } else {
                XtX_inv <- matlib::Ginv(XtX_inv)
            }
        } else {
            XtX_inv <- diag(k+1)
        }
        print(det(XtX_inv))
        U <- summary(fit)$sigma^2 * XtX_inv + U # issue inverting this matrix?
    }
    colnames(Q) <- names(coefficients(fit))
    
    Qbar <- apply(Q, 2, mean)
    Ubar <- U / M
    se_beta <- diag(Ubar)
    B <- matrix(data = rep(0, (k+1)^2), nrow = k+1, ncol = k+1)
    for (m in 1:M) {
        B <- B + (Q[m,] - Qbar) %*% t(Q[m,] - Qbar)
    }
    B <- 1/(M-1) * B
    B_beta <- diag(B)
    
    abs_det_Ubar <- abs(det(Ubar)) # issue inverting this matrix?
    if (!is.na(abs_det_Ubar)) {
        if (abs_det_Ubar > 1e-15) {
            UI <- solve(Ubar)
        }
        else {
            UI <- matlib::Ginv(Ubar)
        }
    }
    else {
        UI <- diag(5)
    }
    
    nu <- (1 + 1 / M) * tr(B %*% UI) / k
    T_var <- (k * (1 + nu) * Ubar)
    
    if (!is.na(abs(det(T_var)))) {
        if (abs(det(T_var)) > 1e-15) {
            T_inv <- solve(T_var)
        } else {
            T_inv <- matlib::Ginv(T_var)
        }
    } else {
        T_inv <- diag(5)
    }
    
    T_beta <- se_beta + (1 + 1/M) * B_beta
    
    eta_moe <- (M-1) * (T_beta / ((1 + 1/M) * B_beta))^2
    MoE <- qt(.975, eta_moe) * sqrt(T_beta)
    
    Qdiff <- Qbar - Q0
    
    test_stat <- colSums(Qdiff * (T_inv %*% Qdiff))
    
    t <- k * (M - 1)
    eta <- ifelse(t > 4, 
                  4 + (t - 4) * (1 + (1 - 2 / t) / nu)^2,
                  t * (1 + 1 / k) * (1 + 1 / nu)^2 / 2)
    
    pval <- pf(test_stat, k, eta, lower.tail = F)
    
    res <- list(Qbar = Qbar, Ubar = Ubar, B = B, Tvar = T_var, Fstat = test_stat, pval = pval, df = c("df1" = k, "df2" = eta), se.beta = se_beta, MoE = MoE)
    return(res)
}
