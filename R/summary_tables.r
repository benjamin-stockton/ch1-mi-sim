# summary_tables.R

round_mean_3 <- function(x) {
    return(round(mean(x, na.rm = T), 3))
}

create_long_format_results <- function(res) {
    res_longer <- res |> 
        #filter(Analysis != "Norm theta") |>
        group_by(Analysis, DGP, reg_relation, N, M, prop_miss) |>
        mutate(
            beta_0_par_val = beta_0,
            beta_1_par_val = beta_1,
            beta_2_par_val = beta_2,
            beta_3_par_val = beta_3,
            beta_4_par_val = beta_4,
            
            beta_0_estimate = beta_hat_0,
            beta_1_estimate = beta_hat_1,
            beta_2_estimate = beta_hat_2,
            beta_3_estimate = beta_hat_3,
            beta_4_estimate = beta_hat_4,
            
            beta_0_moe = MoE_0,
            beta_1_moe = MoE_1,
            beta_2_moe = MoE_2,
            beta_3_moe = MoE_3,
            beta_4_moe = MoE_4,
            
            beta_0_CI_LB = beta_hat_0 - MoE_0,
            beta_0_CI_UB = beta_hat_0 + MoE_0,
            
            beta_1_CI_LB = beta_hat_1 - MoE_1,
            beta_1_CI_UB = beta_hat_1 + MoE_1,

            beta_2_CI_LB = beta_hat_2 - MoE_2,
            beta_2_CI_UB = beta_hat_2 + MoE_2,
            
            beta_3_CI_LB = beta_hat_3 - MoE_3,
            beta_3_CI_UB = beta_hat_3 + MoE_3,

            beta_4_CI_LB = beta_hat_4 - MoE_4,
            beta_4_CI_UB = beta_hat_4 + MoE_4,
        ) |>
        select(-c(beta_0, beta_1, beta_2, beta_3, beta_4,
                starts_with("beta_hat"),
                starts_with("MoE_"), 
                pval, F_stat, starts_with("r_"))) |> 
        tidyr::pivot_longer(
            cols = starts_with("beta_"),
            names_to = c("term", "metric"),
            names_pattern = "beta_(.)_(.+)"
        ) |> 
        mutate(
            term = paste0("beta_", term)
        ) |>
        tidyr::pivot_wider(
            names_from = metric,
            values_from = value,
            values_fn = list
        ) |>
        tidyr::unnest_longer(
            col = c(par_val, estimate, moe, CI_LB, CI_UB)) |>
        arrange(term, Analysis)
        
    return(res_longer)
}

create_full_summary_df <- function(res_longer) {
    smry <- res_longer |> filter(!is.na(MoE)) |>
        group_by(DGP, Analysis, reg_relation, Coef, N, M, prop_miss) |>
        summarize(
            n_sims = n(),
            mean_est = mean(est),
            bias = mean(est - true),
            mse = mean((est - true)^2),
            CI_width = mean(CI_UB - CI_LB),
            CI_Cov = mean(CI_LB <= true & CI_UB >= true) * 100,
            .groups = "keep"
        )
    return(smry)
}

create_kable_table <- function(sum_tab) {
    k_tab <- sum_tab |> #t() |>
        # knitr::kable(align = "lccc")
        kableExtra::kbl(format = "html", escape = F) |>
        kableExtra::kable_paper("striped", full_width = F) |>
        # kableExtra::kable_classic() |>
        kableExtra::pack_rows("MCAR", 1, 6) |>
        kableExtra::pack_rows("MAR", 7, 12) |>
        kableExtra::pack_rows("MNAR", 13, 18) |> 
        kableExtra::add_header_above(c(" " = 2, "$\\beta_1$" = 3, "$\\beta_2$" = 3, "$\\beta_3$" = 3, "$\\beta_4$" = 3))
    # kableExtra::add_header_above(c(" " = 1, "MAR" = 6, "MCAR" = 6, "MNAR" = 6))# |>
    # kableExtra::pack_rows("$\\beta_1 = 0$", 2, 4) |>
    # kableExtra::pack_rows("$\\beta_2 = 0$", 5, 7) |>
    # kableExtra::pack_rows("$\\beta_3 = -7$", 8, 10) |>
    # kableExtra::pack_rows("$\\beta_4 = 6$", 11, 13)
    return(k_tab)
}

print_mean_mse_table <- function(smry, dgp, digits = 2) {
    smry |> 
        filter(DGP == dgp) |>
        tidyr::pivot_wider(
            names_from = Coef,
            values_from = c(mean_est, mse, bias, CI_width, CI_Cov)
        ) |>
        ungroup() |>
        arrange(DGP, reg_relation, Analysis) |> 
        mutate_if(is.numeric, round, digits = digits) |>
        mutate("Reg. Rel." = reg_relation,
               "$\\hat{\\beta}_0$ (MSE)" = paste0(mean_est_beta_0, " (", mse_beta_0, ")"),
               "$\\hat{\\beta}_1$ (MSE)" = paste0(mean_est_beta_1, " (", mse_beta_1, ")"),
               "$\\hat{\\beta}_2$ (MSE)" = paste0(mean_est_beta_2, " (", mse_beta_2, ")"),
               "$\\hat{\\beta}_3$ (MSE)" = paste0(mean_est_beta_3, " (", mse_beta_3, ")"),
               "$\\hat{\\beta}_4$ (MSE)" = paste0(mean_est_beta_4, " (", mse_beta_4, ")")
        ) |>
        select(
            -c(n_sims, DGP,
                starts_with("CI_Cov"), 
                starts_with("CI_width"),
                reg_relation,
                starts_with("mean_est"), 
                starts_with("mse_"),
                starts_with("bias"))) -> m_tab
    
    m_tab |> 
        filter(`Reg. Rel.` == "Simple") |> 
        select(-`Reg. Rel.`) |>
        knitr::kable(format = "latex", 
            booktabs = T, 
            escape = F, 
            digits = digits,
            caption = "Mean table - Simple Relationship") |> 
        print()
    
    m_tab |> filter(`Reg. Rel.` == "Complex") |> 
        select(-`Reg. Rel.`) |>
        knitr::kable(format = "latex", 
            booktabs = T, 
            escape = F, 
            digits = digits,
            caption = "Mean table - Complex Relationship") |>
        print()
}

print_bias_table <- function(smry, dgp, digits = 2) {
    smry |> filter(DGP == dgp) |>
        tidyr::pivot_wider(
            names_from = Coef,
            values_from = c(mean_est, bias, mse, CI_width, CI_Cov)
        ) |>
        ungroup() |>
        arrange(DGP, reg_relation, Analysis) |> 
        mutate_if(is.numeric, round, digits = digits) |>
        mutate("Reg. Rel." = reg_relation,
               "$bias(\\hat{\\beta}_0$) (MSE)" = ifelse(CI_Cov_beta_0 >= 80,
                                                        paste0(bias_beta_0, " (", mse_beta_0, ")"), "--"),
               "$bias(\\hat{\\beta}_1$) (MSE)" = ifelse(CI_Cov_beta_1 >= 80,
                                                        paste0(bias_beta_1, " (", mse_beta_1, ")"), "--"),
               "$bias(\\hat{\\beta}_2$) (MSE)" = ifelse(CI_Cov_beta_2 >= 80,
                                                        paste0(bias_beta_2, " (", mse_beta_2, ")"), "--"),
               "$bias(\\hat{\\beta}_3$) (MSE)" = ifelse(CI_Cov_beta_3 >= 80,
                                                        paste0(bias_beta_3, " (", mse_beta_3, ")"), "--"),
               "$bias(\\hat{\\beta}_4$) (MSE)" = ifelse(CI_Cov_beta_4 >= 80,
                                                        paste0(bias_beta_4, " (", mse_beta_4, ")"), "--")
        ) |>
        select(-c(n_sims, DGP, 
            starts_with("CI_Cov"), 
            starts_with("CI_width"),
            reg_relation, 
            starts_with("mean_est"), 
            starts_with("mse_"), 
            starts_with("bias"))) -> b_tab
    
    b_tab |> filter(`Reg. Rel.` == "Simple") |> 
        select(-`Reg. Rel.`) |>
        knitr::kable(format = "latex", 
            booktabs = T, 
            escape = F, 
            digits = digits,
            caption = 'Bias Table - Simple Relationship') |> print()
    b_tab |> filter(`Reg. Rel.` == "Complex") |> 
        select(-`Reg. Rel.`) |>
        knitr::kable(format = "latex", 
            booktabs = T, 
            escape = F, 
            digits = digits,
            caption = 'Bias Table - Complex Relationship') |> print()
}

smry_tab <- function(res_tab) {
    imp_err_plt <- ggplot(res[which(res$Analysis != "Complete"),], aes(Analysis, median_imp_error, fill = prop_miss)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
        facet_grid(reg_relation~N) +
        # geom_jitter(width = .05) +
        scale_color_viridis_c(option = "B")
    
    beta_box <- ggplot(res_longer, aes(x = reg_relation, y = est)) +
        geom_boxplot(aes(fill = paste0("N_", N, "_p_", prop_miss))) +
        facet_grid(Coef ~ Analysis, scales = "free") +
        # theme(axis.text.x = element_blank())
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    moe_box <- ggplot(res_longer, aes(x = reg_relation, y = MoE)) +
        geom_boxplot(aes(fill = paste0("N_", N, "_p_", prop_miss))) +
        facet_grid(Coef ~ Analysis, scales = "free") +
        # theme(axis.text.x = element_blank())
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    smry_tab <- res_tab |> group_by(reg_relation, N, prop_miss, Analysis) |>
        summarise(across(starts_with("beta_hat_"), round_mean_3, .names = "{.col}_mean"),
                  
                  beta_hat_1_mse = round_mean_3((beta_hat_1 - beta_1)^2),
                  beta_hat_2_mse = round_mean_3((beta_hat_2 - beta_2)^2),
                  beta_hat_3_mse = round_mean_3((beta_hat_3 - beta_3)^2),
                  beta_hat_4_mse = round_mean_3((beta_hat_4 - beta_4)^2),
                  
                  beta_hat_1_CI_Cov = 100*round_mean_3(beta_hat_1 - MoE_1 <= beta_1 & beta_hat_1 + MoE_1 >= beta_1),
                  beta_hat_2_CI_Cov = 100*round_mean_3(beta_hat_2 - MoE_2 <= beta_2 & beta_hat_2 + MoE_2 >= beta_2),
                  beta_hat_3_CI_Cov = 100*round_mean_3(beta_hat_3 - MoE_3 <= beta_3 & beta_hat_3 + MoE_3 >= beta_3),
                  beta_hat_4_CI_Cov = 100*round_mean_3(beta_hat_4 - MoE_4 <= beta_4 & beta_hat_4 + MoE_4 >= beta_4),
                  
                  beta_hat_1_CI_width = round_mean_3(2 * MoE_1),
                  beta_hat_2_CI_width = round_mean_3(2 * MoE_2),
                  beta_hat_3_CI_width = round_mean_3(2 * MoE_3),
                  beta_hat_4_CI_width = round_mean_3(2 * MoE_4),
                  
                  Joint_CI_Cov = 100*round_mean_3((beta_hat_1 - MoE_1 <= beta_1 & beta_hat_1 + MoE_1 >= beta_1) &
                                                      (beta_hat_2 - MoE_2 <= beta_2 & beta_hat_2 + MoE_2 >= beta_2) &
                                                      (beta_hat_3 - MoE_3 <= beta_3 & beta_hat_3 + MoE_3 >= beta_3) &
                                                      (beta_hat_4 - MoE_4 <= beta_4 & beta_hat_4 + MoE_4 >= beta_4)),
                  .groups = "keep")
    
    plt_dat <- smry_tab |> 
        tidyr::pivot_longer(
            cols = matches("beta_hat_"),
            names_to = c("Coef", "metric"),
            names_pattern = "beta_hat_(.)_(.+)"
        ) |> 
        mutate(
            Coef = paste0("beta_", Coef)
        ) |>
        tidyr::pivot_wider(
            names_from = metric,
            values_from = value
        ) |>
        arrange(Coef, Analysis) #|> filter(N == 500)
    
    # Plot of CI widths and coverages
    ci_plt1 <- ggplot(plt_dat[which(plt_dat$reg_relation == "Simple"),], aes(fill = CI_width)) +
        geom_col(aes(x = paste0("N_", N, "_p_", prop_miss), y = CI_Cov), position = "dodge") +
        geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
        geom_hline(yintercept = 95, linetype = "solid", color = "red") +
        facet_grid(Coef ~ Analysis, scales = "free") +
        scale_fill_viridis_c(option = "A")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ci_plt2 <- ggplot(plt_dat[which(plt_dat$reg_relation == "Complex"),], aes(fill = CI_width)) +
        geom_col(aes(x = paste0("N_", N, "_p_", prop_miss), y = CI_Cov), position = "dodge") +
        geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
        geom_hline(yintercept = 95, linetype = "solid", color = "red") +
        facet_grid(Coef ~ Analysis, scales = "free") +
        scale_fill_viridis_c(option = "A")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Plot of joint CI Cov
    jt_plt <- ggplot(plt_dat) +
        geom_col(aes(x = reg_relation, y = Joint_CI_Cov, fill = N), position = "dodge") +
        geom_hline(yintercept = 90, linetype = "dotted", color = "red") +
        geom_hline(yintercept = 95, linetype = "solid", color = "red") +
        facet_grid(Analysis ~ paste0("N_", N, "_p_", prop_miss)) +
        scale_fill_viridis_c() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # get the top analysis method and its metrics
    
    top_method <- plt_dat |> group_by(reg_relation, Coef) |>
        arrange(Coef, mse) |>
        filter(Analysis != "Complete" & CI_Cov >= 85 & Coef %in% c("beta_3", "beta_4") & Joint_CI_Cov > 50) |> 
        slice_min(n = 2, order_by = CI_width)
    return(list(summary_table = smry_tab, summary_longer = plt_dat, joint_CI_plt = jt_plt, CI_cov_plt1 = ci_plt1, CI_cov_plt2 = ci_plt2, top_method = top_method, dat_long = res_tab_long, beta_box = beta_box, moe_box = moe_box, imp_err_plt = imp_err_plt))
}

generate_summary <- function(name_DGP, sim_Q, cca_only) {
    res <- load_simulation_results(fps, cca_only = cca_only)
    print(colnames(res))
    # ci_plots <- draw_ci_plots(res, beta = beta)
    
    smry_tab <- smry_tab(res)
    smry_kbl <- smry_tab$top_method |> kableExtra::kbl() |> kableExtra::kable_styling(full_width = F)
    
    est_mse_plt <- cowplot::plot_grid(smry_tab$beta_box, smry_tab$moe_box, labels = "AUTO", nrow = 2)
    right_plt <- cowplot::plot_grid(smry_tab$CI_cov_plt1, smry_tab$CI_cov_plt2, rel_heights = c(1,1), labels = c("B", "C"), nrow = 2)
    ci_plts <- cowplot::plot_grid(smry_tab$joint_CI_plt, right_plt, nrow = 1, rel_widths = c(2,1))
    
    return(list(smry_tab = smry_tab, smry_kbl = smry_kbl, est_mse_plt = est_mse_plt, ci_plts = ci_plts))
}

