# multiple_imputation_linear_regression.R

make_mice_method_matrix <- function(method_matrix, impute_on_angle, method) {
    if (impute_on_angle) {
        # bpnreg: Impute with the bpnreg projected normal regression from my imputeangles package
        # vmreg:  Impute with circglmbayes version of vM regression from my imputeangles package
        # vmbrms: Impute with brms version of vM regression
        method_matrix["Xc"] <- "~cos(theta)"
        method_matrix["Xs"] <- "~sin(theta)"
        method_matrix["theta"] <- method
    } else {
        method_matrix["theta"] <- "~atan2(Xs, Xc)"
    }
    return(method_matrix)
}

make_mice_pred_matrix <- function(pred_matrix, impute_on_angle) {
    if (impute_on_angle) {
        # Use the (cos, sin) for any of the imputations, but impute on theta with the projected normal regression
        pred_matrix["theta", c("Xc", "Xs")] <- 0
        pred_matrix[, "theta"] <- 0
        pred_matrix[c("Xc", "Xs"), "theta"] <- 1
    } else {
        # Don't use the angle for any of the imputations, it's just there for the graphics
        pred_matrix[, "theta"] <- 0
    }
    return(pred_matrix)
}

multiply_impute <- function(data_inc, impute_type = "norm", impute_on_angle = T, M = 5, printFlag = F) {
    data_inc$theta <- my_atan2(data_inc$Xs, data_inc$Xc)
    mis <- which(is.na(data_inc$theta))
    
    method2 <- ifelse(impute_type %in% c("bpnreg", "pnregid", "pnreggen", "vmreg", "vmrbms"),
                      "pmm", impute_type)
    
    print(impute_type)
    ini <- mice::mice(data_inc, 
                      maxit = 0, 
                      printFlag = F, 
                      method = method2)
    meth <- make_mice_method_matrix(ini$method,
                               impute_on_angle = impute_on_angle, 
                               method = impute_type)
    pred_mat <- make_mice_pred_matrix(ini$predictorMatrix,
                                      impute_on_angle = impute_on_angle)
    
    imps <- mice::mice(data_inc,
                       m = M,
                       printFlag = printFlag,
                       maxit = 1,
                       method = meth,
                       predictorMatrix = pred_mat)
    
    long.imps <- mice::complete(imps, 
                                action = "long",
                                include = T)
    long.imps$miss_pattern <- ifelse(long.imps$.id %in% mis, "Imputed Angles", "Complete")
    
    return(list(imps = imps, long.imps = long.imps))
}

