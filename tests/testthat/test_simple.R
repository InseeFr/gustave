
context("simple")

technical_data_ict <- list(
  samp = list(
    id = ict_sample$firm_id,
    precalc = var_srs(y = NULL, pik = 1 / ict_sample$w_sample, strata = ict_sample$division)
  ),
  nrc = list(
    id = ict_sample$firm_id[ict_sample$resp],
    response_prob = ict_sample$response_prob_est[ict_sample$resp],
    samp_weight = ict_sample$w_sample[ict_sample$resp]
  ),
  calib = list(
    id = ict_sample$firm_id[ict_sample$calib],
    precalc = rescal(y = NULL, 
                     x = as.matrix(ict_sample[
                       ict_survey$firm_id,
                       c(paste0("N_", 58:63), paste0("turnover_", 58:63))
                       ]),
                     w = ict_survey$w_calib[order(ict_survey$firm_id)]
    )
  )
)


y <- matrix(ict_survey$speed_quanti, dimnames = list(ict_survey$firm_id))


test_that("var_simple works", {
  expect_error(
    with(technical_data_ict, var_simple(y, samp = samp, nrc = nrc, calib = calib)), 
    regexp = NA
  )
})

test_that("a variance wrapper can be manually defined on top of var_simple", {
  expect_error({
    variance_wrapper_ict <- define_variance_wrapper(
      variance_function = var_simple,
      reference_id = ict_survey$firm_id,
      reference_weight = ict_survey$w_calib,
      technical_data = technical_data_ict,
      default_id = "firm_id"
    )
    variance_wrapper_ict(ict_survey, speed_quanti)
  },
  regexp = NA
  )
})

test_that("inconsitency detection works as expected", {
  expect_error(
    define_simple_wrapper(), 
    regexp = "A data file"
  )
  expect_error(
    define_simple_wrapper(data = blabla), 
    regexp = "An identifier of the units must be provided"
  )
  expect_error(
    define_simple_wrapper(data = blabla, id = "blabla"), 
    regexp = "Sampling weights must be provided"
  )
  expect_error(
    define_simple_wrapper(
      data = blabla, id = "blabla",
      samp_weight = "blabla",
      nrc_weight = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - weights after non-response"
  )
  expect_error(
    define_simple_wrapper(
      data = blabla, id = "blabla", 
      samp_weight = "blabla",
      resp = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - a variable indicating responding units"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "blabla", 
      samp_weight = "blabla",
      calib_dummy = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - a variable indicating the units taking part"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "blabla", 
      samp_weight = "blabla",
      calib_weight = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - calibrated weights are provided"
  )
})

test_that("welcome message works as expected", {
  skip("skip")
  welcome <- "Variance wrapper definition using the dataset : blabla\n\nThe following features are taken into account:"
  expect_message(
    define_simple_wrapper(
      data = blabla,
      samp_weight = "blabla"
    ), regexp = paste0(welcome, "\n  - simple random sampling WITHOUT stratification")
  )
  expect_message(
    define_simple_wrapper(
      data = blabla,
      samp_weight = "blabla", strata = "blabla"
    ), regexp = tmp <- paste0(welcome, "\n  - stratified simple random sampling")
  )
  expect_message(
    define_simple_wrapper(
      data = blabla,
      samp_weight = "blabla", strata = "blabla",
      scope_dummy = "blabla"
    ), regexp = tmp <- paste0(tmp, "\n  - out-of-scope units")
  )
  expect_message(
    define_simple_wrapper(
      data = blabla,
      samp_weight = "blabla", strata = "blabla",
      scope_dummy = "blabla",
      nrc_weight = "blabla", resp = "blabla"
    ), regexp = tmp <- paste0(tmp, "\n  - non-response correction through reweighting")
  )
  expect_message(
    define_simple_wrapper(
      data = blabla,
      samp_weight = "blabla", strata = "blabla",
      scope_dummy = "blabla",
      nrc_weight = "blabla", resp = "blabla",
      calib_weight = "blabla", calib_var = "blabla"
    ), regexp = paste0(tmp, "\n  - calibration on margins")
  )
})

test_that("argument validity controls work as expected", {
  expect_error(
    define_simple_wrapper(data = blabla, id = "blabla", samp_weight = "blabla"), 
    regexp = "obj"
  )
  expect_error(
    define_simple_wrapper(data = matrix(1:10), id = "blabla", samp_weight = "blabla"), 
    regexp = "data argument must refer to a data.frame"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", 
      samp_weight = "w_sample", strata = "division"
    ),
    regexp = NA
  )
  expect_error(
    define_simple_wrapper(
      data = tibble::as.tibble(ict_sample), id = "firm_id", 
      samp_weight = "w_sample", strata = "division"
    ),
    regexp = NA
  )
  expect_error(
    define_simple_wrapper(
      data = data.table::as.data.table(ict_sample), id = "firm_id",
      samp_weight = "w_sample", strata = "division"
    ),
    regexp = NA
  )
  expect_error(
    define_simple_wrapper(data = ict_sample, id = "blabla", samp_weight = c("blabla", "bloblo")),
    regexp = "The following arguments do not refer to a variable name"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "blabla", samp_weight = "blabla",
      calib_weight = "blabla", calib_var = 2
    ), 
    regexp = "The following arguments do not refer to a vector of variable names"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample", strata = "division",
      nrc_weight = "w_nrc", resp_dummy = "blabla"
    ),
    regexp = "Some variables do not exist in ict_sample: \n  - resp_dummy argument: blabla"
  )
})

test_that("argument value controls work as expected", {
  
  # id
  ict_sample$firm_id[1] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample"
    )
  }, regexp = "contain any missing \\(NA\\) values.")
  rm(ict_sample)
  ict_sample$firm_id[1] <- ict_sample$firm_id[2]
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample"
    )
  }, regexp = "contain any duplicated values.")
  rm(ict_sample)

  # samp_weight
  ict_sample$w_sample <- as.character(ict_sample$w_sample)
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample"
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)
  ict_sample$w_sample[1] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample"
    )
  }, regexp = "contain any missing \\(NA\\) values.")
  rm(ict_sample)

  # strata
  ict_sample$division <- as.numeric(ict_sample$division)
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample", strata = "division"
    )
  }, regexp = " should be of type factor or character.")
  rm(ict_sample)
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample", strata = "division"
    )
  }, regexp = NA)
  ict_sample$division[1] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample", strata = "division"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # scope_dummy
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      scope_dummy = "division"
    )
  }, regexp = "should be of type logical or numeric.")
  ict_sample$scope <- c(FALSE, rep(TRUE, NROW(ict_sample) - 1))
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      strata = "division", scope_dummy = "scope"
    )
  }, regexp = NA)
  ict_sample$scope[1] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      scope_dummy = "scope"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # resp_dummy
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "division"
    )
  }, regexp = "should be of type logical or numeric.")
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      strata = "division", nrc_weight = "w_nrc", resp_dummy = "resp"
    )
  }, regexp = NA)
  ict_sample$resp[1] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # nrc_weight
  ict_sample$w_nrc <- as.character(ict_sample$w_nrc)
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp"
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)
  ict_sample$w_nrc[match(TRUE, ict_sample$resp)] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp"
    )
  }, regexp = "should not contain any missing \\(NA\\) values for responding units.")
  rm(ict_sample)
  ict_sample$w_nrc[match(FALSE, ict_sample$resp)] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      strata = "division", nrc_weight = "w_nrc", resp_dummy = "resp"
    )
  }, regexp = NA)
  rm(ict_sample)
  
  # calib_dummy
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_dummy = "division", calib_var =  c("N_58", "N_59")
    )
  }, regexp = "should be of type logical or numeric.")
  ict_sample$calib <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_dummy = "calib", calib_var =  c("N_58", "N_59")
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # calib_weight
  ict_sample$w_calib <- as.character(ict_sample$w_calib)
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_dummy = "calib", calib_var =  c("N_58", "N_59")
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)
  ict_sample$w_calib[match(TRUE, ict_sample$calib)] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_dummy = "calib", calib_var =  c("N_58", "N_59")
    )
  }, regexp = "should not contain any missing \\(NA\\) values for units used in the calibration process.")
  rm(ict_sample)
  ict_sample$w_calib[match(FALSE, ict_sample$calib)] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      strata = "division", nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_dummy = "calib", calib_var =  c("N_58", "N_59")
    )
  }, regexp = NA)
  rm(ict_sample)
  ict_sample$calib[match(TRUE, ict_sample$calib)] <- FALSE
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id", samp_weight = "w_sample", 
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_dummy = "calib", calib_var =  c("N_58", "N_59")
    )
  }, regexp = "For the responding units not used in the calibration process,")
  rm(ict_sample)

  # calib_var
  ict_sample$complex <- complex(real = 1:NROW(ict_sample), imaginary = 1:NROW(ict_sample))
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample", strata = "division",
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_var =  "complex"
    )
  }, regexp = "The following calibration variables are neither quantitative")
  rm(ict_sample)
  ict_sample[match(TRUE, ict_sample$calib), c("N_58", "N_59")] <- NA
  expect_error({
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      samp_weight = "w_sample", strata = "division",
      nrc_weight = "w_nrc", resp_dummy = "resp",
      calib_weight = "w_calib", calib_var =  c("N_58", "N_59")
    )
  }, regexp = "contain missing \\(NA\\) values for units used in the calibration process:")
  rm(ict_sample)
  
  
  
})