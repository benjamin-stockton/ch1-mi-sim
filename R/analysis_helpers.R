# analysis_helpers.R
library(circular)
library(dplyr)
library(ggplot2)
library(mvtnorm)
library(magrittr)
library(imputeangles) # my package
library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
library(posterior)
library(bayesplot)
library(latex2exp)
color_scheme_set("brightblue")

my_atan2 <- function(s,c) {
    # val <- dplyr::case_when(
    #     c == 0 & s > 0 ~ pi/2,
    #     c > 0 & s >= 0 ~ atan2(s,c),
    #     c < 0 ~ atan2(s,c) + pi,
    #     c >= 0 & s < 0 ~ atan2(s,c) + 2*pi,
    #     TRUE ~ NA_real_
    # )
    val <- atan2(s,c) %% (2*pi)
    return(val)
}

my_acos <- function(x,y,z) {
    if (z != 0) {
        return(atan2(z, sqrt(x^2 + y^2)))
    }
    else if (z == 0 & x != 0 & y != 0) {
        return(pi / 2)
    }
    else {
        return(NA)
    }
}

tr <- function(mat) {
    return(sum(diag(mat)))
}

mean_res_len <- function(X) {
    C <- mean(X[,1]); S <- mean(X[,2])
    return(sqrt(C^2 + S^2))
}
