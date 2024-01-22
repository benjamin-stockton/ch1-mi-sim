# generate_data_for_linear_regression.R

# rabeley <- function(n, mu, kappa, lambda, alpha, beta) {
#     dat <- matrix(numeric(2*n), ncol = 2)
#     colnames(dat) <- c("theta", "x")
#     for (i in 1:n) {
#         t1 <- rwrappedcauchy(1, mu = circular(0), tanh(kappa/2))
#         U <- runif(1)
#         t <- ifelse(U < (1 + lambda * sin(t1)) / 2, t1, t1) + mu
#         scale_par <- beta * (1 - tanh(kappa) * cos(mu-t))^(1/alpha)
#         x <- rweibull(1, shape = alpha, scale = 1/scale_par)
#         dat[i,] <- c(t %% (2*pi), x)
#     }
#     return(dat)
# }

univar_proj_norm_DGP <- function(N, mu, sigma_mat) {
    sigma_mat <- matrix(sigma_mat, nrow = 2)
    theta <- circular::rpnorm(n = N, mu = circular::circular(mu), sigma = sigma_mat)
    return(theta)
}

univar_wrap_norm_DGP <- function(N, mu, sigma_circ = 0.1) {
    mu <- atan2(mu[2], mu[1])
    theta <- circular::rwrappednormal(n = N, mu = circular::circular(mu), sd = sigma_circ)
    return(theta)
}

univar_vM_DGP <- function(N, mu, kappa = 15) {
    mu <- atan2(mu[2], mu[1])
    theta <- circular::rvonmises(n = N, mu = circular::circular(mu), kappa)
    return(theta)
}

reg_proj_norm_DGP <- function(N, X, B = c(1, -.5, -3, 1)) {
    sigma_mat <- diag(2)
    theta <- numeric(N)
    for (i in 1:N) {
        mu_i <- X[i,] %*% matrix(B, nrow = 2)
        
        xi <- mvtnorm::rmvnorm(1, mu_i, sigma_mat)
        theta[i] <- atan2(xi[2], xi[1])
    }
    return(theta)
}

reg_wrap_norm_DGP <- function(N, X, mu, sigma_circ = 0.1) {
    theta <- numeric(N)
    mu <- atan2(mu[2], mu[1])
    for (i in 1:N) {
        mu_i <- (mu + (X[i, ] %*% c(.3,-.3))[1,1]) %% (2*pi)
        theta[i] <- circular::rwrappednormal(n = 1, mu = circular::circular(mu_i), sd = sigma_circ)
    }
    return(theta)
}

reg_vM_DGP <- function(N, X, mu, kappa) {
    theta <- numeric(N)
    X_cs <- X
    X_cs[,1] <- (X[,1] - mean(X[,1])) / sd(X[,1])
    X_cs[,2] <- (X[,2] - mean(X[,2])) / sd(X[,2])
    mu <- atan2(mu[2], mu[1])
    for (i in 1:N) {
        eta <- X_cs[i,1] * -0.1 + X_cs[i, 2] * 0.5
        mu_i <- mu + 2 * atan(eta)
        theta[i] <- circular::rvonmises(1, circular(mu_i), kappa)
    }
    return(theta)
}

# reg_abeley_DGP <- function(N, X, mu, kappa, lambda, a, b) {
#     
# }

theta_DGP <- function(name_DGP, N, mu, sigma_mat = c(1,0,0,1), X = NULL, kappa = 15) {
    if (name_DGP == "PN") {
        theta <- univar_proj_norm_DGP(N, mu, sigma_mat)
    }
    else if (name_DGP == "PNreg") {
        B <- c(3, -1, -1, 4)
        theta <- reg_proj_norm_DGP(N, X, B)
    }
    else if (name_DGP == "WN") {
        theta <- univar_wrap_norm_DGP(N, mu, sigma_circ = 0.1)
    }
    else if (name_DGP == "WNreg") {
        theta <- reg_wrap_norm_DGP(N, X, mu, sigma_circ = 0.1)
    }
    else if (name_DGP == "vM") {
        theta <- univar_vM_DGP(N, mu, kappa = kappa)
    }
    else if (name_DGP == "vMreg") {
        theta <- reg_vM_DGP(N, X, mu, kappa = kappa)
    }
    # else if (name_DGP == "abeley") {
    #     theta <- rabeley(N, mu = mu, kappa = .75, lambda = -1, alpha = 1, beta = 0.5)[,1]
    # }
    else {
        theta <- runif(N, min = 0, max = 2*pi)
    }
    return(theta)
}

generate_data <- function(name_DGP, N = 100, sigma = 5, beta = c(0,0,0,0), mu = c(10,10), sigma_mat = c(1,0,0,1), kappa = 15) {
    # Simulate Data
    num_dat <- mvtnorm::rmvnorm(N, mean = c(0, 2), sigma = matrix(c(1, -2, -2, 10), nrow = 2))
    sigma_mat <- matrix(sigma_mat, nrow = 2)
    theta <- theta_DGP(name_DGP, N = N, mu = mu, sigma_mat = sigma_mat, X = num_dat, kappa = kappa) |> as.numeric()
    
    sim.dat <- data.frame("X1" = num_dat[,1],
                          "X2" = num_dat[,2])
    sim.dat$Xc <- as.numeric(cos(theta))
    sim.dat$Xs <- as.numeric(sin(theta))
    if (length(beta) == 6) {
        sim.dat$Xc2 <- as.numeric(cos(2 * theta))
        sim.dat$Xs2 <- as.numeric(sin(2 * theta))
    }
    sim.dat$Y <- (3 + as.matrix(sim.dat) %*% beta + rnorm(N, 0, sigma))[,1]
    sim.dat$theta <- theta
    return(sim.dat)
}

impose_missing <- function(data, alpha =  c(0, -5, 2, 0, 0, -5), p_miss = .5) {
    alpha[1] <- -log(1/p_miss - 1) - alpha[2:6] %*% apply(data[,c("X1", "X2", "Xc", "Xs", "Y")], 2, mean)
    N <- nrow(data)
    ls <- (rep(alpha[1], N) + as.matrix(data[,c("X1", "X2", "Xc", "Xs", "Y")]) %*% alpha[2:6])[,1]
    ps <- (1 + exp(-ls))^-1
    mis <- sample(1:nrow(data), size = floor(p_miss * nrow(data)), prob = ps)
    
    # mis <- which(mis.pat == 1)
    dat.inc <- data
    dat.inc[mis, c("Xc", "Xs", "theta")] <- NA
    
    dat.mis <- dat.inc[mis,]
    dat.obs <- dat.inc[-mis,]
    return(list(mis_ind = mis, dat.inc = dat.inc, dat.obs = dat.obs, dat.mis = dat.mis))
}
