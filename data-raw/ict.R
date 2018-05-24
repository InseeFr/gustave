



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
pryr::object_size(ict_pop)
sum(ict_pop$employees < 10)
tapply(ict_pop$turnover, ict_pop$division, summary)

# Sample
id_sample <- sampling::strata(
  data = ict_pop,
  stratanames = "division",
  size = n,
  method = "srswor"
)$ID_unit
ict_sample <- ict_pop[id_sample, ]
ict_sample$w_sample <- unname((N / n)[ict_sample$division])

  
# Non-response generation and correction
ict_sample$response_prob <- pmax(0, pmin(1, 
  response_prob_div[ict_sample$division] + 
    response_prob_turnover * ict_sample$turnover + 
    rnorm(NROW(ict_sample), 0, 0.1)
))
ict_sample$resp <- ict_sample$response_prob > runif(NROW(ict_sample))
ict_sample <- ict_sample[, setdiff(names(ict_sample), "response_prob")]
ict_sample$hrg <- with(ict_sample, paste0(
  division, "_", as.integer(cut(ict_sample$turnover, c(-Inf, quantile(ict_sample$turnover, c(0.2, 0.4, 0.6, 0.8)), Inf)))
))
table(ict_sample$resp, ict_sample$hrg)
response_prob_est <- sapply(split(ict_sample, ict_sample$hrg), function(hrg) with(hrg, sum(w_sample) / sum(w_sample * resp)))
ict_sample$response_prob_est <- response_prob_est[ict_sample$hrg]
ict_sample$w_nr <- ict_sample$w_sample * ict_sample$response_prob_est



# Calibration
ict_survey <- ict_sample[ict_sample$resp, setdiff(names(ict_sample), c("hrg", "resp", "response_prob_est"))]
calib_var <- c(paste0("N_", division), paste0("turnover_", division))
ict_survey$N_58 <- (ict_survey$division == "58") * 1
ict_survey$N_59 <- (ict_survey$division == "59") * 1
ict_survey$N_60 <- (ict_survey$division == "60") * 1
ict_survey$N_61 <- (ict_survey$division == "61") * 1
ict_survey$N_62 <- (ict_survey$division == "62") * 1
ict_survey$N_63 <- (ict_survey$division == "63") * 1
ict_survey$turnover_58 <- ict_survey$N_58 * ict_survey$turnover
ict_survey$turnover_59 <- ict_survey$N_59 * ict_survey$turnover
ict_survey$turnover_60 <- ict_survey$N_60 * ict_survey$turnover
ict_survey$turnover_61 <- ict_survey$N_61 * ict_survey$turnover
ict_survey$turnover_62 <- ict_survey$N_62 * ict_survey$turnover
ict_survey$turnover_63 <- ict_survey$N_63 * ict_survey$turnover
calib_total <- with(ict_pop, c(table(division), tapply(turnover, division, sum)))
names(calib_total) <- calib_var
ict_survey$w_calib <- ict_survey$w_nr * sampling::calib(
  Xs = ict_survey[, calib_var], 
  d = ict_survey$w_nr,
  total = calib_total,
  method = "raking"
)


# Survey variables (without and with NA values)

# - internet connection speed (qualitative polytomous variable)
fiber_prob <- pmin(ict_survey$turnover / 5e4, 1)
fiber <- fiber_prob > runif(NROW(ict_survey))
ict_survey$speed_quanti <- ifelse(fiber, rnorm(NROW(ict_survey), 100, 10), pmax(1, rnorm(NROW(ict_survey), 10, 5)))
ict_survey$speed_quanti_NA <- ict_survey$speed_quanti
ict_survey$speed_quanti_NA[runif(NROW(ict_survey)) < 0.30] <- NA
ict_survey$speed_quali <- cut(
  ict_survey$speed_quanti, c(-Inf, 2, 10, 30, 100, Inf),
  c("Less than 2 Mbs", "Between 2 and 10 Mbs", "Between 10 and 30 Mbs", "Between 30 and 100 Mbs", "Above 100 Mbs")
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
rownames(ict_pop) <- ict_pop$firm_id
rownames(ict_sample) <- ict_sample$firm_id
rownames(ict_survey) <- ict_survey$firm_id
save(ict_pop, file = "data/ict_pop.RData")
save(ict_sample, file = "data/ict_sample.RData")
save(ict_survey, file = "data/ict_survey.RData")
rm(list = ls(all.names = TRUE))