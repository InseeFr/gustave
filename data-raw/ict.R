rm(list = ls(all.names = TRUE))



# Information and communication sector

# Parameters
set.seed(456789123)
division <- c("58", "59", "60", "61", "62", "63")
N <- c("58" = 1790, "59" = 950, "60" = 170, "61" = 380, "62" = 3700, "63" = 680)
employees <- c("58" = 56, "59" = 32, "60" = 163, "61" = 362, "62" = 76, "63" = 56)
turnover <- c("58" = 14000, "59" = 9000, "60" = 64000, "61" = 162000, "62" = 14600, "63" = 11400)
n <- c("58" = 200, "59" = 100, "60" = 50, "61" = 50, "62" = 200, "63" = 50)
response_prob_div <- c("58" = 0.90, "59" = 0.80, "60" = 0.75, "61" = 0.85, "62" = 0.95, "63" = 0.85)
response_prob_turnover <- 1e-5

# Population
ict_pop <- do.call(rbind, lapply(division, function(d){
  # d <- "58"
  df <- data.frame(division = rep(d, N[d]), stringsAsFactors = FALSE)
  df$employees <- rgamma(N[d], shape = 0.3)
  df$employees <- round(df$employees * (employees[d] + rnorm(NROW(df), -10, 5)) / base::mean(df$employees)) + 10
  df$turnover <- rgamma(N[d], shape = 0.5)
  df$turnover <- df$turnover * (turnover[d] + rnorm(NROW(df), 0, 500)) / base::mean(df$turnover)
  df
}))
ict_pop$firm_id <- paste0("X", formatC(1:NROW(ict_pop), flag = 0, width = ceiling(log10(NROW(ict_pop)))))
ict_pop <- ict_pop[, c("firm_id", "division", "employees", "turnover")]
ict_pop$strata <- ict_pop$division
ict_pop$strata[ict_pop$employees >= 1000] <- "EXH"
n_exh <- table(ict_pop$division[ict_pop$strata == "EXH"])
n[names(n_exh)] <- n[names(n_exh)] - n_exh
n <- c(n, EXH = sum(n_exh))
N <- table(ict_pop$strata)
ict_pop <- ict_pop[order(ict_pop$strata), ]
tapply(ict_pop$turnover, ict_pop$strata, summary)

# Sample
id_sample <- sampling::strata(
  data = ict_pop,
  stratanames = "strata",
  size = n,
  method = "srswor"
)$ID_unit
ict_sample <- ict_pop[id_sample, ]
table(ict_sample$strata)
ict_sample$w_sample <- as.vector((N / n)[ict_sample$strata])

# scope
ict_sample$scope <- (1 - pmax(15 - ict_sample$employees, 0)/60) >= rnorm(NROW(ict_sample))
# Note: some units fall under the 10 employees threshold before the survey

# Non-response generation
ict_sample$response_prob <- pmax(0, pmin(1, 
  response_prob_div[ict_sample$division] + 
    response_prob_turnover * ict_sample$turnover + 
    rnorm(NROW(ict_sample), 0, 0.1)
))
ict_sample$resp <- ict_sample$scope & ict_sample$response_prob > runif(NROW(ict_sample))
ict_sample <- ict_sample[, setdiff(names(ict_sample), "response_prob")]

# Non-response correction
ict_sample$no_reweighting <- with(ict_sample, scope & employees >= 1000 & turnover >= 1e5)
# Note: The biggest firms (1,000 employees and 100M turnover or over)
# are not reweighted. When non-respondent, their value is corrected 
# through imputation.
ict_sample$nrc <- ict_sample$scope & !ict_sample$no_reweighting
ict_sample$hrg[ict_sample$nrc] <- with(ict_sample, paste0(
  strata, "_", as.integer(cut(ict_sample$turnover, c(-Inf, median(ict_sample$turnover), Inf)))
))[ict_sample$nrc]
table(ict_sample$resp, ict_sample$hrg)
response_prob_est <- sapply(
  split(ict_sample[ict_sample$nrc, ], ict_sample$hrg[ict_sample$nrc]), 
  function(hrg) with(hrg, sum(w_sample * resp) / sum(w_sample))
)
ict_sample$response_prob_est[ict_sample$nrc] <- response_prob_est[ict_sample$hrg[ict_sample$nrc]]
ict_sample$w_nrc <- with(ict_sample, ifelse(no_reweighting, w_sample, w_sample / ict_sample$response_prob_est))


# Calibration
ict_sample$calib <- !ict_sample$no_reweighting
# Note: Out-of-scope units do participate in calibration

# - calibration variables
calib_var <- c(paste0("N_", division), paste0("turnover_", division))
ict_sample$N_58 <- (ict_sample$division == "58") * 1
ict_sample$N_59 <- (ict_sample$division == "59") * 1
ict_sample$N_60 <- (ict_sample$division == "60") * 1
ict_sample$N_61 <- (ict_sample$division == "61") * 1
ict_sample$N_62 <- (ict_sample$division == "62") * 1
ict_sample$N_63 <- (ict_sample$division == "63") * 1
ict_sample$turnover_58 <- ict_sample$N_58 * ict_sample$turnover
ict_sample$turnover_59 <- ict_sample$N_59 * ict_sample$turnover
ict_sample$turnover_60 <- ict_sample$N_60 * ict_sample$turnover
ict_sample$turnover_61 <- ict_sample$N_61 * ict_sample$turnover
ict_sample$turnover_62 <- ict_sample$N_62 * ict_sample$turnover
ict_sample$turnover_63 <- ict_sample$N_63 * ict_sample$turnover

# - calibration margins (taking the non-reweighted units into account)
calib_N <- table(ict_pop$division)
calib_N_no_reweighting <- table(ict_sample$division[ict_sample$no_reweighting])
calib_N[names(calib_N_no_reweighting)] <- calib_N[names(calib_N_no_reweighting)] - calib_N_no_reweighting
calib_turnover <- with(ict_pop, tapply(turnover, division, sum))
calib_turnover_no_reweighting <-
  with(subset(ict_sample, no_reweighting), tapply(turnover, division, sum))
calib_turnover[names(calib_turnover_no_reweighting)] <- 
  calib_turnover[names(calib_turnover_no_reweighting)] - calib_turnover_no_reweighting
calib_total <- c(calib_N, calib_turnover)
names(calib_total) <- calib_var

# - implement calibration
ict_sample$w_nrc[ict_sample$calib & is.na(ict_sample$w_nrc)] <-
  ict_sample$w_sample[ict_sample$calib & is.na(ict_sample$w_nrc)]
ict_sample$w_calib[ict_sample$calib] <- 
  ict_sample$w_nrc[ict_sample$calib] * sampling::calib(
    Xs = ict_sample[ict_sample$calib, calib_var], 
    d = ict_sample$w_nrc[ict_sample$calib],
    total = calib_total,
    method = "raking"
  )
ict_sample$w_calib[!ict_sample$calib] <- ict_sample$w_sample[!ict_sample$calib]

# Survey variables (without and with NA values)

ict_sample$dissemination <- ict_sample$resp | ict_sample$no_reweighting
ict_survey <- ict_sample[ict_sample$dissemination, c("firm_id", "division", "employees", "turnover", "w_calib")]

# - internet connection speed (qualitative polytomous variable)
fiber_prob <- pmin(ict_survey$turnover / 5e4, 1)
fiber <- fiber_prob > runif(NROW(ict_survey))
ict_survey$speed_quanti <- ifelse(fiber, rnorm(NROW(ict_survey), 100, 10), pmax(1, rnorm(NROW(ict_survey), 10, 5)))
ict_survey$speed_quanti_NA <- ict_survey$speed_quanti
ict_survey$speed_quanti_NA[runif(NROW(ict_survey)) < 0.30] <- NA
ict_survey$speed_quali <- cut(
  ict_survey$speed_quanti, c(-Inf, 2, 10, 30, 100, Inf),
  c("Less than 2 Mbps", "Between 2 and 10 Mbps", "Between 10 and 30 Mbps", "Between 30 and 100 Mbps", "Above 100 Mbps")
)
ict_survey$speed_quali_NA <- ict_survey$speed_quali
ict_survey$speed_quali_NA[is.na(ict_survey$speed_quanti_NA)] <- NA

# - big_data usage (qualitative dichotomous variable)
big_data_prob <- pmin(ict_survey$turnover / 1e6 + fiber * 0.20, 1)
ict_survey$big_data <- big_data_prob > runif(NROW(ict_survey))
big_data_NA_prob <- ifelse(ict_survey$big_data, 0.05, 0.25)
ict_survey$big_data_NA <- ict_survey$big_data
ict_survey$big_data_NA[runif(NROW(ict_survey)) < big_data_NA_prob] <- NA

# Export
ict_sample$no_reweighting <- NULL
rownames(ict_pop) <- ict_pop$firm_id
rownames(ict_sample) <- ict_sample$firm_id
rownames(ict_survey) <- ict_survey$firm_id
ict_pop <- ict_pop[order(ict_pop$firm_id), ]
ict_sample <- ict_sample[order(ict_sample$firm_id), ]
ict_survey <- ict_survey[order(ict_survey$firm_id), ]
save(ict_pop, file = "data/ict_pop.RData", compress = "xz")
save(ict_sample, file = "data/ict_sample.RData", compress = "xz")
save(ict_survey, file = "data/ict_survey.RData", compress = "xz")
rm(list = ls(all.names = TRUE))