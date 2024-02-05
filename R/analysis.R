lm_analysis <- function(imp_data) {
    fit <- with(imp_data, lm(Y ~ X1 + X2 + U1 + U2))
    
    if (is.mira(fit)) {
        res_pool <- pool(fit)
        res2 <- res_pool$pooled
        res2$se <- sqrt(res2$t)
        return(res2)
    }
    else if (class(fit) == "lm") {
        smry <- summary(fit)
        
        res <- as.data.frame(smry$coefficients)
        colnames(res) <- c("estimate", "se", "t_val", "p_val")
        res$term <- rownames(res)
        res$df <- nrow(imp_data) - 5
        rownames(res) <- 1:nrow(res)
        return(res)
    }
}
