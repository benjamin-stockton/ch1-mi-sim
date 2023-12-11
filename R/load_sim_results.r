# load_sim_results.R

make_file_paths <- function(sim_name, name_DGP, fp_prefix, simQ = 100, 
        M = 10, N = 100, gamma = 0.25, theta_reg = FALSE, mu = c(10, 0), 
        Sigma = c(1,0,0,1), beta = c(0,0,-7, 6), alpha = c(0,0,0,0,0,0)) {
    
    fp <- paste(fp_prefix, name_DGP,
                paste0("p_miss_", gamma, "_N_", N),
                paste0("mu_", stringr::str_c(mu, collapse = "_"),
                       "_sigma_mat_", 
                            stringr::str_c(Sigma, collapse = "_")),
                paste0("OLS_sim_iters_", simQ, "_M_", M,
                       "_beta_", stringr::str_c(beta, collapse = "_"),
                       "_alpha_", stringr::str_c(alpha, collapse = "_")),
                 sep = "/")
    
    print(paste0("Output path: ", fp))
    return(fp)
}

build_fp_list <- function(sim_name, name_DGP, fp_prefix, fps = list("MAR" = NA),
        simQ = 100, M = 10, N = 100, gamma = 0.25, theta_reg = FALSE,
        mu = c(10, 0), Sigma = c(1,0,0,1),
        beta = c(0,0,-7, 6), alphas = c(0,0,0,0,0,0)) {
    
    for (m in names(fps)) {
        fps[[m]] <- make_file_paths(sim_name = sim_name, 
                                    name_DGP = name_DGP, 
                                    fp_prefix = fp_prefix,
                                    simQ = simQ, 
                                    M = M, N = N,
                                    gamma = gamma, theta_reg = theta_reg,  
                                    mu = mu, Sigma = Sigma, 
                                    beta = beta, alpha = alphas)
    }
    return(fps)
}

fig_file_paths <- function(sim_name, simQ = 100, M = 10, N = 100, 
        gamma = 0.25, theta_reg = FALSE, mu = c(10, 0), 
        Sigma = c(1,0,0,1), beta = c(0,0,-7, 6), alpha = c(0,0,0,0,0,0)) {
    fps <- list("CI" = NA, 
                "NHST" = NA, 
                "beta_violins" = NA, 
                "imp_by_y" = NA, 
                "circ_imp" = NA, 
                "mult_imp" = NA)
    
    plt_suffix <- c("CIs_plots", "NHST_hists", "beta_violins",
                    "imputations_by_Y", "circ_imputations", 
                    "mult_imputations")
    
    for (i in 1:3) {
        fname <- paste0("OLS_sim_iters_", simQ,
                        "_M_", M,
                        "_N_", N, 
                        "_gamma_", gamma,
                        "_theta_reg_", theta_reg, 
                        "_mu_", stringr::str_c(mu, collapse = "_"),
                        "_Sigma_", stringr::str_c(Sigma, collapse = "_"), 
                        "_beta_", stringr::str_c(beta, collapse = "_"),
                        "_alpha_", stringr::str_c(alpha, collapse = "_"), 
                        "_", plt_suffix[i], ".png")
        fps[[i]] <- file.path("Sim_Results", sim_name, fname)
    }
    
    for (i in 4:6) {
        fname <- paste0("MI_plots_M_", M, 
                        "_N_", N, 
                        "_gamma_", gamma,
                        "_theta_reg_", theta_reg, 
                        "_mu_", stringr::str_c(mu, collapse = "_"),
                        "_Sigma_", stringr::str_c(Sigma, collapse = "_"), 
                        "_beta_", stringr::str_c(beta, collapse = "_"),
                        "_alpha_", stringr::str_c(alpha, collapse = "_"), 
                        "_", plt_suffix[i], ".png")
        fps[[i]] <- file.path("Sim_Results", sim_name, fname)
    }
    return(fps)
}

read_results_files <- function(fps, include_cca_only = FALSE) {
    miss_types <- names(fps)
    res <- list()
    for (i in miss_types) {
        if (include_cca_only) {
            fp_cca <- paste0(fps[[i]], "_CCA_results.csv")
            
            te2 <- try (read.csv(fp_cca))
            if (class(te2) != 'try-error') {
                tmp2 <- read.csv(fp_cca)
                tmp2 <- tmp2 #|> 
                            # dplyr::select(-c(beta_hat_0, MoE_0, beta_0))
            }
            
            if (class(te) != 'try-error' & class(te2) != 'try-error') {
                tmp <- dplyr::bind_rows(tmp, tmp2)
            }
            else if (class(te) == 'try-error' & class(te2) != 'try-error') {
                tmp <- tmp2
            }
        }
        else {
            fp_mi <- paste0(fps[[i]], "_results.csv")
            
            te <- try (read.csv(fp_mi))
            if (class(te) != 'try-error') {
                tmp <- read.csv(fp_mi)
            }
        }
        
        if (class(te) != 'try-error') {
            tmp$MISSTYPE <- i
            res[[i]] <- tmp
        }
        
    }
    res <- dplyr::bind_rows(res, .id = "column_label")
    return(res)
}

load_simulation_results <- function(sim.settings, fp_prefix = "Sim_Results") {
    res_list <- list()
    for (i in 1:nrow(sim.settings)) {
        fps <- build_fp_list(sim_name = sim.settings[i, "DGP"], 
                    name_DGP = sim.settings[i, "DGP_Name"],
                    fp_prefix = fp_prefix,
                    fps = list("MAR" = NA),
                    simQ = sim.settings[i, "simQ"],
                    M = sim.settings[i, "M"],
                    N = sim.settings[i, "N"], 
                    gamma = sim.settings[i, "p_miss"],
                    theta_reg = F,
                    mu = sim.settings[i, c("mu_1", "mu_2")],
                    Sigma = sim.settings[i, c("sigma_mat_1", "sigma_mat_2",
                                            "sigma_mat_3", "sigma_mat_4")],
                    beta = sim.settings[i, c("beta_1", "beta_2", 
                                            "beta_3", "beta_4")],
                    alphas = sim.settings[i,
                                        c("alpha_1", "alpha_2", "alpha_3",
                                        "alpha_4", "alpha_5", "alpha_6")]
        )
        
        tmp <- read_results_files(fps, include_cca_only = FALSE)
        tmp$reg_relation <- ifelse(sim.settings[i, "beta_1"] == 0, 
                                    "Simple", "Complex")
        tmp <- tmp |>
            select(-c(starts_with("r_"), 
                starts_with("MISSTYPE")))
        tmp <- tmp |>
            mutate(
            DGP = sim.settings[i, "DGP_Name"],
            beta_1_true = sim.settings[i, "beta_1"],
            beta_2_true = sim.settings[i, "beta_2"],
            beta_3_true = sim.settings[i, "beta_3"],
            beta_4_true = sim.settings[i, "beta_4"]
            )
        
        res_list[[i]] <- tmp
    }
            

    res <- bind_rows(res_list)
    return(res)
}
