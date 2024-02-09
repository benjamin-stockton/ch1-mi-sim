impute <- function(data, l_method = "pmm", c_method = "pmm", M = 3, maxit = 5) {
    data <- data |> dplyr::select(theta, U1, U2, X1, X2, Y)
    
    imp0 <- mice(data, method = l_method, m = 1, maxit = 0)
    
    mthd <- imp0$method
    
    p_mat <- imp0$predictorMatrix
    
    if (c_method == "jav-norm") {
        mthd["theta"] <- "~atan2(U2, U1)"
        mthd["U1"] <- "norm"
        mthd["U2"] <- "norm"
        
        p_mat[,"theta"] <- 0
        p_mat["theta", ] <- 0
        p_mat["theta", c("U1", "U2")] <- 1
    }
    
    else if (c_method == "jav-pmm") {
        mthd["theta"] <- "~atan2(U2, U1)"
        mthd["U1"] <- "pmm"
        mthd["U2"] <- "pmm"
        
        p_mat[,"theta"] <- 0
        p_mat["theta", ] <- 0
        p_mat["theta", c("U1", "U2")] <- 1
    }
    
    else {
        mthd["theta"] <- c_method
        mthd["U1"] <- "~cos(theta)"
        mthd["U2"] <- "~sin(theta)"
        
        p_mat[,"theta"] <- 0
        p_mat[c("U1", "U2"), ] <- 0
        p_mat[c("U1", "U2"), "theta"] <- 1
        p_mat["theta", c("U1", "U2")] <- 0
    }
    
    # print(mthd)
    
    invisible(
        capture.output(
            imp <- mice(data, m = M, method = mthd, predictorMatrix = p_mat,
                maxit = maxit, printFlag = FALSE)
        )
    )
    
    return(imp)
}
