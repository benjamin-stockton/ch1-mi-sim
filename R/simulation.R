library(mice)
library(imputeangles)
library(pnregstan)

# file_path <- file.path("R", "pn_arx_imp_sim_study")
# source(file.path(file_path, "generate_data.R"))
# source(file.path(file_path, "impute.R"))
# source(file.path(file_path, "analysis.R"))
# source(file.path(file_path, "utils.R"))
source(file.path("generate_data.R"))
source(file.path("impose_missing.R"))
source(file.path("impute.R"))
source(file.path("analysis.R"))
source(file.path("utils.R"))

# From command line get the following arguments
N_sim <- 3 # Number of simulation iterations
N_sample <- 100 # Sample size
init_seed <- 1234 # Initial seed
M <- 2 # Number of imputations
pop_pars <- list(
    mu_0 = c(0,0),
    B_vec = c(1, 3, 0, -5),
    Sigma_vec = c(1,0,0,1),
    kappa = 15,
    beta_y = c(5, 1, 0, 0.5, -0.25),
    sigma_y = 0.5,
) # population parameters to draw samples from
miss_pars <- list(
    freq = c(1),
    mech = "MAR",
    p_miss = 0.5
) # Missingness mechanism parameters (also controls MAR/MNAR)

methods <- c("complete", "cca", "bpnreg", "vmreg", "pnregid", "jav-pmm", "pmm")

# seeds <- matrix(NA, nrow = N_sim, ncol = 626)
# set.seed(init_seed)

# pars <- pop_pars, miss_pars, beta_hat, se_beta_hat, df_beta_hat, fmi, p_miss
# total_pars <- length(pop_pars) + length(miss_pars) + 4*P + 1

x1 <- parallel::mclapply(1:N_sim,
               mc.cores = 20,
                function(x) {
    print(x)

   # seeds[1,] <- get.seed()
                    generate_data <- function(name_DGP, N = 100, sigma = 5, beta = c(0,0,0,0), mu = c(10,10), sigma_mat = c(1,0,0,1), kappa = 15)
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
   prop_miss <- apply(as.matrix(inc_data[,2:5]), 2, function(i) {mean(is.na(i))})
   
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
       
       res$par_val <- pop_pars$beta_y
       res$p_miss <- c(0, prop_miss)
       res$iter <- x
       res$method <- mtd
       return(res)
   })
   
   iter_res |> 
       dplyr::bind_rows() -> results
   
   return(results)
})

out_path <- file.path("..", "..", "sim-results")


saveRDS(x1, file = paste0(out_path, "/sim-results_", Sys.Date(), ".rds"))

x1 |> 
    dplyr::bind_rows() |>
    readr::write_csv(paste0(out_path, "/sim-results_", Sys.Date(), ".csv"))

# x1 <- readRDS("../../sim-results/sim-results_2023-11-30.rds")
# 
# x1 |> dplyr::bind_rows()
