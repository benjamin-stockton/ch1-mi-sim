library(purrr)
library(dplyr)
library(ggplot2)
library(rsimsum)
source("R/summary_tables.r")


simsum_plots <- function(res_sum, var, true_val) {
    s0 <- res_sum[which(res_sum$term == var),]
    
    s0 <- s0 |>
        simsum(estvarname = "estimate", se = "se", true = true_val, df = "df",
               methodvar = "Analysis", ref = "Complete", 
               by = c("set_n"), x = TRUE)
    
    smry_0 <- summary(s0)
    
    p0 <- autoplot(s0, type = "est_ridge")
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

create_res_sum <- function(ll, load_rds = FALSE, file_prefix = "sim-results_", file_suffix = ".csv", in_dir = "sim-results", out_dir = "sim-summary") {
    res_sum <-
        map_df(ll,
               .f = function(sc) {
                   x1 <- readr::read_csv(file.path(in_dir, paste0(file_prefix, sc, file_suffix))) |>
                       mutate(reg_relation = c("simple", "complex")[sc])
                   
                   x2 <- create_long_format_results(x1) |>
                       mutate(
                           df = NA,
                           se = NA
                       )
                   
                   x3 <- x2 |>
                       group_by(DGP, Analysis, term, reg_relation) |>
                       mutate(
                           term = case_when(
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
                               is.na(df) ~ N-6,
                               TRUE ~ NA
                           ),
                           se = case_when(
                               !is.na(se) ~ se,
                               is.na(se) ~ moe / qt(0.975, df = df),
                               TRUE ~ NA
                           ),
                           lb95 = estimate - moe,
                           ub95 = estimate + moe,
                           cov = case_when(
                               lb95 > par_val ~ 0,
                               ub95 < par_val ~ 0,
                               TRUE ~ 1
                           )
                       ) |>
                       mutate(
                           set_n = sc
                       ) |>
                       ungroup() |>
                       select(
                           Analysis, term, estimate, par_val, df, se, moe, cov, set_n, reg_relation
                       ) |>
                       left_join(setting, by = "set_n")
                   
               })
    
    res_sum |>
        group_by(set_n, DGP, Analysis, term) |>
        summarize(
            n_sim = n()
        ) |> 
        filter(term == "intercept") |>
        print(n = 10)
    
    dgp <- res_sum$DGP[1]
    
    f_out <- paste0(file_prefix, dgp)
    print(file.path(out_dir, f_out))
    # saveRDS(res_sum, file = file.path(out_dir, paste0(f_out, ".rds")))
    readr::write_csv(res_sum, file = file.path(out_dir, paste0(f_out, ".csv")))
    
    beta0 <- simsum_plots(res_sum, var = "intercept", true_val = 3)
    beta1 <- simsum_plots(res_sum, var = "X1", true_val = 0)
    beta2 <- simsum_plots(res_sum, var = "X2", true_val = 0)
    beta3 <- simsum_plots(res_sum, var = "Xc", true_val = 1)
    beta4 <- simsum_plots(res_sum, var = "Xs", true_val = 0.5)
    
    return(list(beta0 = beta0,
                beta1 = beta1,
                beta2 = beta2,
                beta3 = beta3,
                beta4 = beta4))
}

# simsum still assumes that the same parameter values are used across the simulation settings ie reg-relations are the same

in_dir <- "sim-results"
file_prefix <- "sim-results-mar-mi-lm-sim-setting-"
file_suffix <- ".csv"
setting <- readRDS("sim_settings/test-setting.rds")
ll <- 1:nrow(setting)

res <- create_res_sum(ll = ll, in_dir = in_dir, file_prefix = file_prefix,
                      file_suffix = file_suffix)

lapply(res, function(betaj) {betaj$p_grid})
