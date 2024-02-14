library(mice, warn.conflicts = F, quietly = T)
library(imputeangles)
library(ggplot2)
library(dplyr)

file_path <- file.path("R")
source(file.path(file_path, "generate_data.R"))
source(file.path(file_path, "impose_missing.R"))
source(file.path(file_path, "impute.R"))
source(file.path(file_path, "analysis.R"))
source(file.path(file_path, "utils.R"))

# From command line get the following arguments
set_n <- 6 # simulation setting id number
N_sim <- 250 # Number of simulation iterations
N_sample <- 1000 # Sample size
init_seed <- 987 # Initial seed
M <- 50 # Number of imputations
name_DGP <- "PN Reg"
pop_pars <- list(
    mu_0 = c(1, 1),
    B_vec = c(-0.15, -0.35, -0.09, -0.40),
    Sigma_vec = c(1, 1, 1, 1),
    kappa = 15,
    beta_y = c(-0.55, 0, -0.2, 0.3),
    sigma_y = 3.5
) # population parameters to draw samples from
miss_pars <- list(
    freq = c(1),
    mech = "MAR",
    p_miss = 0.5
) # Missingness mechanism parameters (also controls MAR/MNAR)

methods <- c("complete", "jav-pmm", "pmm", "jav-norm", "norm",
            "bpnreg", "vmreg", "pnregid", "vmbrms", "pnreggen")

# seeds <- matrix(NA, nrow = N_sim, ncol = 626)
set.seed(init_seed * set_n)

x1 <- parallel::mclapply(1:N_sim,
                        mc.cores = 125,
                        function(x) {
    print(x)
    
    # seeds[1,] <- get.seed()
    sample_data <- generate_data(name_DGP = name_DGP, N = N_sample,
                                    sigma = pop_pars$sigma_y,
                                    beta = pop_pars$beta_y, 
                                    mu = pop_pars$mu_0, 
                                    sigma_mat = pop_pars$Sigma_vec,
                                    kappa = pop_pars$kappa)
    
    inc_data <- impose_missingness(sample_data, 
                                freq = miss_pars$freq, 
                                mech = miss_pars$mech, 
                                p_miss = miss_pars$p_miss)
    prop_miss <- apply(as.matrix(inc_data), 2, function(i) {mean(is.na(i))})
    
    iter_res <- lapply(methods, function(mtd) {
        if (mtd == "complete") {
            res <- lm_analysis(sample_data)
        }
        else if (mtd == "cca") {
            res <- lm_analysis(inc_data)
        }
        else {
            imp_data <- impute(inc_data, l_method = "pmm", c_method = mtd, M = M, maxit = 1)
            
            res <- lm_analysis(imp_data)
        }

        res$par_val <- c(3,pop_pars$beta_y)
        res$p_miss <- c(0,prop_miss[c("X1", "X2", "U1", "U2")])
        res$iter <- x
        res$method <- mtd
        return(res)
    })
    
    results <- iter_res |> 
        dplyr::bind_rows()


    return(results)
})
    

saveRDS(x1, file = paste0(out_path, "/", "mar-pn-reg-", "mi-lm-sim_setting-", set_n, ".rds"))

results <- x1 |> dplyr::bind_rows()

out_path <- file.path("sim-results")

x2 <- x1 |> 
    dplyr::bind_rows()
f_out <- paste0(out_path, "/", "mar-pn-reg-", "mi-lm-sim_setting-", set_n, ".csv")
if (file.exists(f_out)) {
    readr::write_csv(x2, f_out, append = TRUE)
} else {
    readr::write_csv(x2, f_out)
}
