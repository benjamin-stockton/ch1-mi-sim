impose_missingness <- function(df, freq = c(1), mech = "MAR", p_miss = 0.5) {
    mads_df <- ampute(data = df)
    
    
    amp_p <- mads_df$patterns[1,]
    amp_p[1,] <- 1
    amp_p[1, c("U1", "U2", "theta")] <- 0
    # print(amp_p)
    
    amp_w <- mads_df$weights[1,]
    amp_w[1, ] <- 1
    amp_w[1, c("U1", "U2", "theta")] <- 0
    # print(amp_w)
    
    mads_df1 <- ampute(data = df, patterns = amp_p, prop = p_miss,
                       weights = amp_w, freq = freq, mech = mech)
    
    return(mads_df1$amp)
}


