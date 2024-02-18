# PN LC sim 6

sim6 <- readr::read_csv("sim-results/mar-pn-lc-mi-lm-sim_setting-6-pt1.csv")

sim6_2 <- readr::read_csv("sim-results/mar-pn-lc-mi-lm-sim_setting-6-pt2.csv")

df6 <- dplyr::bind_rows(sim6, sim6_2)

readr::write_csv(df6, "sim-results/mar-pn-lc-mi-lm-sim_setting-6.csv")

# PN Reg sim 6

sim6 <- readRDS("sim-results/mar-pn-reg-mi-lm-sim_setting-6-pt1.rds")

sim6_2 <- readr::read_csv("sim-results/mar-pn-reg-mi-lm-sim_setting-6-pt2.csv")

lapply(sim6, function(d) {ifelse(is.data.frame(d), 0, 1)}) |> unlist()

sim6[[32]] <- NULL

sim6 <- sim6 |> dplyr::bind_rows()

df6 <- dplyr::bind_rows(sim6, sim6_2)

readr::write_csv(sim6, "sim-results/mar-pn-reg-mi-reg-sim_setting-6-pt1.csv")
readr::write_csv(df6, "sim-results/mar-pn-reg-mi-lm-sim_setting-6.csv")

# PN LC sim 7

sim7 <- readRDS("sim-results/mar-pn-lc-mi-lm-sim_setting-7.rds")

s7 <- lapply(sim7, function(d) {ifelse(is.data.frame(d), 0, 1)}) |> unlist()
(sim_ind <- which(s7 == 1))

sim7 <- lapply(sim7, function(d) {
    if (is.data.frame(d)) {
        return(d)
    } else {
        return(NULL)   
    }
    }) |> 
    dplyr::bind_rows()

readr::write_csv(sim7, "sim-results/mar-pn-lc-mi-lm-sim_setting-7.csv")


# PN Reg sim 7

sim7 <- readRDS("sim-results/mar-pn-reg-mi-lm-sim_setting-7.rds")

s7 <- lapply(sim7, function(d) {ifelse(is.data.frame(d), 0, 1)}) |> unlist()
(null_ind <- which(s7 == 1))

sim7[[16]]
sim7[[61]]

sim7 <- lapply(sim7, function(d) {
    if (is.data.frame(d)) {
        return(d)
    } else {
        return(NULL)   
    }
}) |> 
    dplyr::bind_rows()

readr::write_csv(sim7, "sim-results/mar-pn-reg-mi-lm-sim_setting-7.csv")

# PN lc sim 7

sim7 <- readRDS("sim-results/mar-pn-lc-mi-lm-sim_setting-7-cca.rds")

s7 <- lapply(sim7, function(d) {ifelse(is.data.frame(d), 0, 1)}) |> unlist()
(null_ind <- which(s7 == 1))

sim7 <- lapply(sim7, function(d) {
    if (is.data.frame(d)) {
        return(d)
    } else {
        return(NULL)   
    }
}) |> 
    dplyr::bind_rows()

sim7$method

readr::write_csv(sim7, "sim-results/mar-pn-lc-mi-lm-sim_setting-7-cca.csv")


# PN Reg sim 7

sim7 <- readRDS("sim-results/mar-pn-reg-mi-lm-sim_setting-7-cca.rds")

s7 <- lapply(sim7, function(d) {ifelse(is.data.frame(d), 0, 1)}) |> unlist()
(null_ind <- which(s7 == 1))

sim7 <- lapply(sim7, function(d) {
    if (is.data.frame(d)) {
        return(d)
    } else {
        return(NULL)   
    }
}) |> 
    dplyr::bind_rows()

readr::write_csv(sim7, "sim-results/mar-pn-reg-mi-lm-sim_setting-7-cca.csv")

# DGP

sim6_1 <- readr::read_csv("sim-results/mar-dgp-mi-lm-sim_setting-6-pt1.csv")
sim6_2 <- readr::read_csv("sim-results/mar-dgp-mi-lm-sim_setting-6-pt2.csv")

sim6 <- dplyr::bind_rows(sim6_1, sim6_2)

readr::write_csv(sim6, "sim-results/mar-dgp-mi-lm-sim_setting-6.csv")
