



# Construction sector
# https://www.insee.fr/fr/statistiques/3303618?sommaire=3353488#tableau-T18F184G3
# Reduction with scale 1 / 100

# Parameters
set.seed(456789123)
division <- c("41", "42", "43")
N <- c("41" = 600, "42" = 60, "43" = 4290)
label <- c("41" = "Construction of buildings", 
           "42" = "Civil engineering", 
           "43" = "Specialised construction activities"
)
turnover <- c("41" = 730, "42" = 330, "43" = 1670)
value_added <- c("41" = 130, "42" = 100, "43" = 630)
n <- c("41" = 48, "42" = 3, "43" = 257)
response_prob_div <- c("41" = 0.90, "42" = 0.80, "43" = 0.85)
response_prob_turnover <- 1e-8

# Population
sbs_pop <- do.call(rbind, lapply(division, function(d){
  df <- data.frame(division = rep(d, N[d]), stringsAsFactors = FALSE)
  df$label <- as.factor(label[df$division])
  df$turnover <- rgamma(N[d], shape = 1)
  df$turnover <- df$turnover * turnover[d] / sum(df$turnover)
  df
}))
sbs_pop$id <- 1:NROW(sbs_pop)
sbs_pop$turnover <- sbs_pop$turnover * 1e6
sbs_pop <- sbs_pop[, c("id", "division", "label", "turnover")]

# Sample
id_sample <- sampling::strata(
  data = sbs_pop,
  stratanames = "division",
  size = n,
  method = "srswor"
)$ID_unit
sbs_sample <- sbs_pop[id_sample, ]
sbs_sample$w_sample <- unname((N / n)[sbs_sample$division])

  
# Non-response and survey data 
sbs_sample$response_prob <- pmax(0, pmin(1, 
  response_prob_div[sbs_sample$division] + 
    response_prob_turnover * sbs_sample$turnover + 
    rnorm(NROW(sbs_sample), 0, 0.1)
))
sbs_sample$resp <- sbs_sample$response_prob > runif(NROW(sbs_sample))
sbs_sample <- sbs_sample[, setdiff(names(sbs_sample), "response_prob")]
sbs_survey <- sbs_sample[sbs_sample$resp, ]
sbs_survey$investment <- sbs_survey$turnover * (rnorm(NROW(sbs_survey), 0.10, 0.01))


# Non-response correction
sbs_sample$hrg <- with(sbs_sample, paste0(
  division, "_", as.integer(cut(sbs_sample$turnover, c(-Inf, quantile(sbs_sample$turnover, c(0.2, 0.4, 0.6, 0.8)), Inf)))
))
nr_corr <- sapply(split(sbs_sample, sbs_sample$hrg), function(hrg) with(hrg, sum(w_sample) / sum(w_sample * resp)))
table(sbs_sample$resp, sbs_sample$hrg)
sbs_sample$w_nr <- sbs_sample$w_sample * nr_corr[sbs_sample$hrg]
sbs_survey <- merge(sbs_survey, sbs_sample[, c("id", "w_nr")])



# Calibration
calib_var <- c("N_41", "N_42", "N_43", "turnover_41", "turnover_42", "turnover_43")
sbs_survey$N_41 <- (sbs_survey$division == "41") * 1
sbs_survey$N_42 <- (sbs_survey$division == "42") * 1
sbs_survey$N_43 <- (sbs_survey$division == "43") * 1
sbs_survey$turnover_41 <- sbs_survey$N_41 * sbs_survey$turnover
sbs_survey$turnover_42 <- sbs_survey$N_42 * sbs_survey$turnover
sbs_survey$turnover_43 <- sbs_survey$N_43 * sbs_survey$turnover
calib_total <- with(sbs_pop, c(table(division), tapply(turnover, division, sum)))
names(calib_total) <- calib_var
sbs_survey$w_calib <- sbs_survey$w_nr * sampling::calib(
  Xs = sbs_survey[, calib_var], 
  d = sbs_survey$w_nr,
  total = calib_total,
  method = "linear"
)
sbs_survey <- sbs_survey[, c(
  "id", "division", "label", "turnover", "investment", 
  "N_41", "N_42", "N_43", "turnover_41", "turnover_42", "turnover_43", "w_calib"
)]

# Export
save(sbs_pop, file = "data/sbs_pop.RData")
save(sbs_sample, file = "data/sbs_sample.RData")
save(sbs_survey, file = "data/sbs_survey.RData")