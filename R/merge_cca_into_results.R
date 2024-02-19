library(readr)
library(dplyr)

in_dir <- "sim-results"
file_prefix <- "mar-pn-lc-mi-lm-sim_setting-"
file_suffix <- ".csv"
cca_suffix <- "-cca.csv"

ll <- c(1:9)

lapply(ll, function(i) {
    d1 <- read_csv(paste0(in_dir, "/", file_prefix, i, file_suffix))
    d2 <- read_csv(paste0(in_dir, "/", file_prefix, i, cca_suffix))
    
    print(table(d1$method))
    print(table(d2$method))
    
    d3 <- bind_rows(d1, d2)
    print(table(d3$method))
    write_csv(d3, paste0(in_dir, "/", file_prefix, i, file_suffix))
    return(paste0("Wrote to ", in_dir, "/", file_prefix, i, file_suffix))
})


in_dir <- "sim-results"
file_prefix <- "mar-pn-reg-mi-lm-sim_setting-"
file_suffix <- ".csv"
cca_suffix <- "-cca.csv"

ll <- c(1:9)

lapply(ll, function(i) {
    d1 <- read_csv(paste0(in_dir, "/", file_prefix, i, file_suffix))
    d2 <- read_csv(paste0(in_dir, "/", file_prefix, i, cca_suffix))
    
    print(table(d1$method))
    print(table(d2$method))
    
    d3 <- bind_rows(d1, d2)
    print(table(d3$method))
    write_csv(d3, paste0(in_dir, "/", file_prefix, i, file_suffix))
    return(paste0("Wrote to ", in_dir, "/", file_prefix, i, file_suffix))
})
