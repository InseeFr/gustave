variance_function <- function(y, niveau = "men"){

  # Calibration
  y <- rescal(y, x = x, w = w)

  # Non-response
  y <- add0(y, rownames = samp$firm_id)
  var_nr <- var_pois(y, pik = samp$response_prob_est, w = samp$w_sample)

  # Sampling
  y <- y / samp$response_prob_est
  var_sampling <- var_srs(y, pik = 1 / samp$w_sample, strata = samp$division)

  var_sampling + var_nr

}

# With x the calibration variables matrix...
x <- as.matrix(ict_survey[
  order(ict_survey$firm_id),
  c(paste0("N_", 58:63), paste0("turnover_", 58:63))
])

# ... w the reference weight (the one after calibration)...
w <- ict_survey$w_calib

# ... and samp the sample file of ict_sample
samp <- ict_sample

# Test of the variance function
y <- as.matrix(ict_survey$speed_quanti)
rownames(y) <- ict_survey$firm_id
variance_function(y)

ident <- list()
ident[["men"]] <- ict_survey$firm_id

# Step 2 : Definition of a variance wrapper

variance_wrapper <- define_variance_wrapper(
  variance_function = variance_function,
  reference_id = quote(ident[[niveau]]),
  default = list(id = "firm_id", weight = "w_calib"),
  objects_to_include = c("x", "w", "samp", "ident")
)
rm(x, w, ident, samp, y, variance_function)

# Step 3 : Features of the variance wrapper

# Better display of results
variance_wrapper(ict_survey, speed_quanti)
