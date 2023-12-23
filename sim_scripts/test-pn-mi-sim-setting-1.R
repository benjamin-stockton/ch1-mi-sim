# pn_mi_simulation_script.R

##########################
# Load Packages
##########################

library(circular, warn.conflicts = F, quietly = T)
library(dplyr, warn.conflicts = F, quietly = T)
library(ggplot2, warn.conflicts = F, quietly = T)
library(mvtnorm, warn.conflicts = F, quietly = T)
library(doParallel, warn.conflicts = F, quietly = T)

file.sources = list.files(c("R/sim-funcs"),
                          pattern="*.R$", full.names=TRUE,
                          ignore.case=TRUE)
invisible(capture.output(sapply(file.sources,source,.GlobalEnv)))
invisible(capture.output(source("R/utils.R")))

######################################
# Data Generation
######################################

N_sim <- 5
name_DGP <- "PN High Conc"

DGP_type <- "PN"
N <- 100
sigma <- 0.5
beta <- c(0, 0, 1, 0.5)
beta0 <- beta
alpha <- c(0,2,1,0,0,5)

M <- 10

mu_i <- c(5,0)
sigma_mat <- c(1, 0, 0, 1)
prop_miss <- 0.1

########################################
# Analysis Simulations
########################################

fp <- file.path("Sim_Results", "Test")
generic_output_path <- paste0("results-mi-sim-setting-", 1, "-reg-rel-", "simple")
print(paste0("Output path: ", fp, "/", generic_output_path))

r1 <- data.frame("tmp" = rnorm(1000))
te <- try(write.csv(r1, 
                         paste0(fp, "/test_", 1, ".csv"),
                         row.names = F))
if (class(te) != 'try-error') {
    print("Directory exists!")
} else {
    print("Directory doesn't exist! Writing to Project root.")
    write.csv(r1, 
                  paste0(fp, "/test_", 1, ".csv"),
                  row.names = F)
}
set.seed(98672)

print(paste0("Executing ", N_sim, " iterations across 15 cores."))

x1 <- parallel::mclapply(1:N_sim,
                   mc.cores = 15,
                   function(x) {
    print(x)
    pn_mi_reg_sim(name_DGP = DGP_type, N_sim = 1, N = N, beta = beta,
                  sigma = sigma, Q0 = c(3,beta), M = M, p_miss = prop_miss,
                  mu_i = mu_i, sigma_mat = sigma_mat,
                  bPN_impute = TRUE, vM_impute = TRUE, alpha = alpha)
})
res <- x1 |> dplyr::bind_rows()

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

if (class(te) != 'try-error') {
    print("Directory exists!")
    readr::write_csv(res, 
              paste0(fp, "/", generic_output_path, ".csv"),
              append = FALSE)
} else {
    print("Directory doesn't exist! Writing to Project root.")
    readr::write_csv(res, 
              paste0(generic_output_path, ".csv"),
              append = FALSE)
}



