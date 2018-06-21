rm(list = ls(all.names = TRUE))

define_simple_wrapper(
  data = ict_sample, id = "firm_id",
  sampling_weight = "w_sample", strata = "division",
  nrc_weight = "w_nr", resp = "resp2"
  # calib_weight = "w_calib", calib_var = c("blabla", "bloblo")
)


variance_function <- function(y, niveau = "men", samp, x, w){

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

# This estimation requires some technical data: 
technical_data <- list(
  
  # x: calibration variables matrix
  x = as.matrix(ict_survey[
    order(ict_survey$firm_id),
    c(paste0("N_", 58:63), paste0("turnover_", 58:63))
  ]),
  
  # w: calibrated weights
  w = ict_survey$w_calib[order(ict_survey$firm_id)],
  
  # samp: sample file
  samp = ict_sample
  
)

# Test of the variance function
y <- matrix(ict_survey$speed_quanti, dimnames = list(ict_survey$firm_id))
with(technical_data, variance_function(y, samp = samp, x = x, w = w))

ident <- list()
ident[["men"]] <- ict_survey$firm_id

# Step 2 : Definition of a variance wrapper

variance_wrapper <- define_variance_wrapper(
  variance_function = variance_function,
  reference_id = quote(ident[[niveau]]),
  technical_data = technical_data,
  default = list(id = "firm_id", weight = "w_calib"),
  arg_type = list(data = "y", tech = c("x", "w", "samp", param = "niveau")),
  objects_to_include = "ident"
)
rm(ident, technical_data)
variance_wrapper(ict_survey, speed_quanti)

ls.str(environment(variance_wrapper))
