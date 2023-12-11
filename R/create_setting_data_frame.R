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
                            beta = beta)
    
    n_settings <- length(N_sample) * length(N_sim) * length(p_miss) * n_beta
    
    setting$beta_name <- rep(rep(paste0("beta_", 1:4), each = n_settings / n_beta), n_beta)
    if (n_beta == 2)
        setting$reg_rel <- rep(c("simple", "complex"), each = 4 * n_settings / n_beta)
    
    setting <- setting |>
            tidyr::pivot_wider(names_from = beta_name, values_from = beta)
    
    
    setting <- setting |>
            dplyr::mutate(set_n = seq(1, length(setting$N_sample), 1),
                          M = dplyr::case_when(
                              p_miss == 0.9 ~ 100,
                              TRUE ~ 100 * p_miss
                          ))
    return(setting)
}