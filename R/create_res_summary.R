library(purrr)
library(dplyr)
library(ggplot2)
library(rsimsum)

# setting <- readRDS("sim_settings/vm-reg-setting.rds")

simsum_plots <- function(res_sum, var, true_val, by = c("N_sample", "p_miss")) {
    s0 <- res_sum[which(res_sum$term2 == var),]
    
    s0 <- s0 |>
        simsum(estvarname = "estimate", se = "se", true = true_val, df = "df",
               methodvar = "method", ref = "complete", 
               by = by, x = TRUE)
    
    smry_0 <- summary(s0)
    
    p0 <- autoplot(s0, type = "est_ridge")
    p0 <- p0 + 
        coord_cartesian(xlim = c(-5, 5))
    p1 <- autoplot(smry_0, type = "lolly", "bias")
    p2 <- autoplot(smry_0, type = "lolly", "cover")
    p3 <- autoplot(smry_0, type = "lolly", "becover")
    
    (p4 <- cowplot::plot_grid(p0, p1, p2, p3, nrow = 2))
    return(list(summary = smry_0,
                p_bias = p1,
                p_cov = p2,
                p_becov = p3,
                p_grid = p4))
}

create_res_sum <- function(ll, load_rds = FALSE, file_prefix = "sim-results_", file_suffix = ".csv", in_dir = "sim-results", out_dir = "sim-summary", by = c("N_sample", "p_miss")) {
    res_sum <-
        map_df(ll,
               .f = function(sc) {
                   x1 <- readr::read_csv(file.path(in_dir, paste0(file_prefix, sc, file_suffix))) |>
                       mutate(
                           set_n = sc
                       )
                   
                   x2 <- x1 |>
                       left_join(setting, by = "set_n") |>
                       group_by(set_n, method, term) |>
                       mutate(
                           term2 = case_when(
                               term == "(Intercept)" ~ "intercept",
                               term == "intercept" ~ "intercept",
                               term == "beta_0" ~ "intercept",
                               term == "beta_1" ~ "X1",
                               term == "beta_2" ~ "X2",
                               term == "beta_3" ~ "Xc",
                               term == "beta_4" ~ "Xs",
                               TRUE ~ term
                           ),
                           df = case_when(
                               !is.na(df) ~ df,
                               is.na(df) ~ -5,
                               TRUE ~ NA
                           ),
                           se = case_when(
                               !is.na(se) ~ se,
                               # is.na(se) ~ moe / qt(0.975, df = df),
                               TRUE ~ NA
                           ),
                           moe = qt(0.975, df) * se,
                           lb95 = estimate - moe,
                           ub95 = estimate + moe,
                           cov = case_when(
                               lb95 > par_val ~ 0,
                               ub95 < par_val ~ 0,
                               TRUE ~ 1
                           )
                       ) |>
                       ungroup() |>
                       select(
                           method, term2, estimate, par_val, df, se, moe, cov, set_n, fmi
                       ) |>
                       left_join(setting, by = "set_n")
                   
               })
    print(res_sum)
    
    r2 <- res_sum |>
        group_by(set_n, name_DGP, method, term2) |>
        summarize(
            n_sim = n()
        ) |> 
        filter(term2 == "intercept") |>
        print(n = 98)
    n_sim_min <- min(r2$n_sim)
    res_sum <- res_sum |>
        group_by(set_n, method, term2) |>
        slice_sample(n = n_sim_min)
    res_sum |>
        group_by(set_n, name_DGP, method, term2) |>
        summarize(
            n_sim = n()
        ) |> 
        filter(term2 == "intercept") |>
        print(n = 99)
    dgp <- res_sum$name_DGP[1]
    
    f_out <- paste0(dgp, file_prefix)
    print(file.path(out_dir, f_out))
    # saveRDS(res_sum, file = file.path(out_dir, paste0(f_out, ".rds")))
    readr::write_csv(res_sum, file = file.path(out_dir, paste0(f_out, ".csv")))
    
    beta0 <- simsum_plots(res_sum, var = "intercept", true_val = 3, by = by)
    beta1 <- simsum_plots(res_sum, var = "X1", true_val = -0.55, by = by)
    beta2 <- simsum_plots(res_sum, var = "X2", true_val = 0, by = by)
    beta3 <- simsum_plots(res_sum, var = "U1", true_val = -0.2, by = by)
    beta4 <- simsum_plots(res_sum, var = "U2", true_val = 0.3, by = by)
    
    return(list(beta0 = beta0,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                beta4 = beta4))
}



