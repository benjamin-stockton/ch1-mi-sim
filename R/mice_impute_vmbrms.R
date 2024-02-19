# mice_impute_vmbrms.R

library(brms)

mice.impute.vmbrms <- function(y, ry, x,...) {
    
    dat <- as.data.frame(cbind(y, x))
    dat$theta <- as.numeric(circular::minusPiPlusPi(circular::circular(dat[,1])))
    dat <- dat[,-1]
    dat_new <- dat[!ry,]
    dat <- dat[ry,]
    
    prior1 <- brms::prior(normal(0, 5), class = b) + 
        brms::prior(normal(0, 5), class = Intercept)
    
    # intercept in the link function
    fit <- brms::brm(theta ~ ., 
                     data = dat, 
                     family = brms::von_mises(link = "tan_half", link_kappa = "log"),
                     prior = prior1,
                     save_model = "Stan_Models/mice_brms_von_mises_regression.stan",
                     file = "Stan_Models/Models/mice_brms_von_mises_regression",
                     silent = 2,
                     refresh = 0,
                     file_refit = "on_change")
    
    theta_imp <- brms::posterior_predict(fit, 
                                         newdata = dat_new,
                                         ndraws = 1, 
                                         summary = FALSE)[1,]
    
    return(theta_imp)
}








