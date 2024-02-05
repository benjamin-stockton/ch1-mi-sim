create_setting_data_frame <- function(N_sample = c(100, 250),
                                      N_sim = c(5),
                                      name_DGP = "PN High Conc",
                                      DGP = "PN",
                                      p_miss = c(0.1, 0.5, 0.9),
                                      beta = c(c(0, 0, 1, 0.5), c(10, 2, 0.5, 0))) {
    
    n_beta <- length(beta) / 4
    setting <- expand.grid(N_sample = N_sample,
                            N_sim = N_sim,
                            DGP = DGP,
                            name_DGP = name_DGP,
                            p_miss = p_miss,
                            beta = beta, 
                           init_seed = 987)
    
    n_settings <- length(N_sample) * length(N_sim) * length(p_miss) * length(name_DGP)
    
    setting$beta_name <- rep(paste0("beta_", 1:4), each = n_settings)
    
    # print(setting)
    if (n_beta == 2)
        setting$reg_rel <- rep(c("simple", "complex"), each = 4 * n_settings / n_beta)
    
    setting <- setting |>
            tidyr::pivot_wider(names_from = beta_name, values_from = beta)
    
    
    setting <- setting |>
            dplyr::mutate(set_n = seq(1, length(setting$N_sample), 1),
                          DGP = dplyr::case_when(
                              name_DGP %in% c("PN Bi modal", "PN Skewed", "PN Low Conc", "PN High Conc") ~ "PN",
                              name_DGP == "PN Reg" ~ "PNreg",
                              name_DGP == "vM Reg" ~ "vMreg",
                              name_DGP == "WN Reg" ~ "WNreg",
                              TRUE ~ name_DGP
                          ),
                          M = dplyr::case_when(
                              p_miss == 0.9 ~ 100,
                              TRUE ~ 100 * p_miss
                          ),
                          mu_1 = dplyr::case_when(
                              name_DGP == "PN Bi modal" ~ 0.5,
                              name_DGP == "PN Skewed" ~ 4,
                              name_DGP == "PN Low Conc" ~ 1,
                              name_DGP == "PN High Conc" ~ 10,
                              TRUE ~ 1
                          ),
                          mu_2 = dplyr::case_when(
                              name_DGP == "PN Bi modal" ~ 0,
                              name_DGP == "PN Skewed" ~ 0,
                              name_DGP == "PN Low Conc" ~ 1,
                              name_DGP == "PN High Conc" ~ 10,
                              TRUE ~ 1
                          ),
                          Sigma_11 = dplyr::case_when(
                              name_DGP == "PN Skewed" ~ 5,
                              TRUE ~ 1
                          ),
                          Sigma_12 = dplyr::case_when(
                              name_DGP %in% c("PN Skewed", "PN Bi modal") ~ -2,
                              TRUE ~ 1
                          ),
                          Sigma_21 = Sigma_12,
                          Sigma_22 = dplyr::case_when(
                              name_DGP == "PN Bi modal" ~ 10,
                              TRUE ~ 1
                          ))
    return(setting)
}

