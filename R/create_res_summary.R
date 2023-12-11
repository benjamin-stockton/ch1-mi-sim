create_res_sum <- function(ll, load_rds = FALSE, file_prefix = "sim-results_", file_suffix = ".csv", in_dir = "sim-results", out_dir = "sim-summary") {
    res_sum <-
        map_df(ll,
               .f = function(sc) {
                   if (load_rds) {
                       x1 <- readRDS(list.files(in_dir, paste0(file_prefix, "-sim_setting-", sc, file_suffix), full.names = T)) |> purrr::list_rbind()
                   }
                   else {
                       x1 <- readr::read_csv(file.path(in_dir, paste0(file_prefix, "-sim_setting-", sc, file_suffix)))
                   }
                       
                   x1 |>
                       group_by(method, term) |>
                       mutate(
                           se = case_when(
                               is.na(std_err) ~ sqrt(t),
                               TRUE ~ std_err
                           ),
                           df2 = case_when(
                               is.na(df) ~ 244,
                               TRUE ~ df
                           ),
                           moe = qt(0.975, df = df2) * se,
                           lb95 = estimate - moe,
                           ub95 = estimate + moe,
                           cov = case_when(
                               lb95 > par_val ~ 0,
                               ub95 < par_val ~ 0,
                               TRUE ~ 1
                           )
                       ) |>
                       summarize(
                           n_sim = n(),
                           true_val = mean(par_val),
                           mean_est = mean(estimate),
                           mean_se = mean(se),
                           avg_bias = mean(estimate - par_val),
                           mse = mean((estimate - par_val)^2),
                           mad = mean(abs(estimate - par_val)),
                           ci_cov = mean(cov),
                           ci_width = mean(moe*2)
                       ) |>
                       mutate(
                           set_n = sc
                       ) |>
                       left_join(setting, by = "set_n")
                   
               })
    
    
    f_out <- paste0("sum_", file_prefix)
    print(file.path(out_dir, f_out))
    saveRDS(res_sum, file = file.path(out_dir, paste0(f_out, ".rds")))
    readr::write_csv(res_sum, file = file.path(out_dir, paste0(f_out, ".csv")))
}


