# beta_ci_plots.R

draw_ci_plots <- function(res_tab, beta) {
    beta1_ci <- beta_ci_plot(res_tab, k = 1, beta = beta)
    beta2_ci <- beta_ci_plot(res_tab, k = 2, beta = beta)
    betac_ci <- beta_ci_plot(res_tab, k = 3, beta = beta)
    betas_ci <- beta_ci_plot(res_tab, k = 4, beta = beta)
    # plt <- cowplot::plot_grid(beta1_ci, beta2_ci, betac_ci, betas_ci, nrow = 4)
    plt <- cowplot::plot_grid(betac_ci, betas_ci, nrow = 2)
    return(plt)
}

beta_ci_plot <- function(res, k = 1, beta = c(0,0,-7,6)) {
    
    res$MISSTYPE <- factor(res$MISSTYPE, levels = c("MCAR", "MAR", "MNAR"))
    res$Analysis <- factor(res$Analysis, levels = c("Complete", "bPNReg",
                                                    "Norm CS", "PMM CS", "Norm theta", "PMM theta"))
    
    tmp <- res %>% group_by(Analysis) %>% arrange(Analysis, MISSTYPE, !!sym(paste0("beta_",k)))
    n_methods <- nrow(tmp[which(tmp$Analysis == "bPNReg" & tmp$MISSTYPE == "MAR"),])
    
    tmp$iter <- rep(1:n_methods, nrow(tmp) / n_methods)
    
    plt <- ggplot(tmp, aes_string("iter", paste0("beta_", k), color = "Analysis")) +
        geom_point(size = .75) +
        geom_line(aes_string("iter", paste0("beta_", k, " - MoE_", k)), color = "red") +
        geom_line(aes_string("iter", paste0("beta_", k, " + MoE_", k)), color = "red") +
        geom_hline(yintercept = beta[k]) +
        facet_grid(MISSTYPE~ Analysis, scales = "free_y") +
        theme(legend.position = "none") +
        labs(
            # title = latex2exp::TeX(paste0("CI Coverages for $\\beta_", k, "$")),
            x = "", y = latex2exp::TeX(paste0("$\\beta_", k, "$")))
    return(plt)
}
