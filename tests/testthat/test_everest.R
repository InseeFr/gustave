
context("everest")

technical_data_ict <- list(
  samp = list(
    id = ict_sample$firm_id,
    exclude = rep(FALSE, NROW(ict_sample)),
    precalc = suppressWarnings(var_srs(
      y = NULL, pik = 1 / ict_sample$w_sample, strata = ict_sample$strata
    ))
  ),
  nrc = list(
    id = ict_sample$firm_id[ict_sample$resp],
    response_prob = ict_sample$response_prob_est[ict_sample$resp],
    sampling_weight = ict_sample$w_sample[ict_sample$resp]
  ),
  calib = list(
    id = ict_sample$firm_id[ict_sample$calib],
    precalc = res_cal(y = NULL, 
                     x = as.matrix(ict_sample[
                       ict_sample$calib,
                       c(paste0("N_", 58:63), paste0("turnover_", 58:63))
                       ]),
                     w = ict_sample$w_calib[ict_sample$calib],
                     id = ict_sample$firm_id
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
    everest(), 
    regexp = "The following arguments are missing: data, id, dissemination_dummy, dissemination_weight, sampling_weight."
  )
  expect_error(
    everest(
      data = blabla, id = "blabla", dissemination_dummy = "blabla", dissemination_weight = "blabla",
      sampling_weight = "blabla",
      nrc_weight = "blabla"
    ), regexp = "weights after non-response"
  )
  expect_error(
    everest(
      data = ict_sample, id = "blabla", dissemination_dummy = "blabla", dissemination_weight = "blabla", 
      sampling_weight = "blabla",
      calibration_dummy = "blabla"
    ), regexp = "a variable indicating the units taking part"
  )
  expect_error(
    everest(
      data = ict_sample, id = "blabla", dissemination_dummy = "blabla", dissemination_weight = "blabla",
      sampling_weight = "blabla",
      calibration_weight = "blabla"
    ), regexp = "calibrated weights are provided"
  )
  expect_error(
    everest(
      data = ict_sample, id = "blabla", dissemination_dummy = "blabla", dissemination_weight = "blabla",
      sampling_weight = "blabla",
      nrc_dummy = "blabla"
    ), regexp = "a variable indicating responding units and/or a variable"
  )
})

test_that("welcome message works as expected", {
  skip("skip")
  welcome <- "Variance wrapper definition using the dataset : blabla\n\nThe following features are taken into account:"
  expect_message(
    everest(
      data = blabla,
      sampling_weight = "blabla"
    ), regexp = paste0(welcome, "\n  - simple random sampling WITHOUT stratification")
  )
  expect_message(
    everest(
      data = blabla,
      sampling_weight = "blabla", strata = "blabla"
    ), regexp = tmp <- paste0(welcome, "\n  - stratified simple random sampling")
  )
  expect_message(
    everest(
      data = blabla,
      sampling_weight = "blabla", strata = "blabla",
      scope_dummy = "blabla"
    ), regexp = tmp <- paste0(tmp, "\n  - out-of-scope units")
  )
  expect_message(
    everest(
      data = blabla,
      sampling_weight = "blabla", strata = "blabla",
      scope_dummy = "blabla",
      nrc_weight = "blabla", resp = "blabla"
    ), regexp = tmp <- paste0(tmp, "\n  - non-response correction through reweighting")
  )
  expect_message(
    everest(
      data = blabla,
      sampling_weight = "blabla", strata = "blabla",
      scope_dummy = "blabla",
      nrc_weight = "blabla", resp = "blabla",
      calibration_weight = "blabla", calibration_var = "blabla"
    ), regexp = paste0(tmp, "\n  - calibration on margins")
  )
})

test_that("argument validity controls work as expected", {
  expect_error(
    everest(
      data = blabla, 
      id = "blabla", dissemination_dummy = "blabla", dissemination_weight = "blabla", 
      sampling_weight = "blabla"
    ), 
    regexp = "obj"
  )
  expect_error(
    everest(
      data = matrix(1:10), 
      id = "blabla", dissemination_dummy = "blabla", dissemination_weight = "blabla",
      sampling_weight = "blabla"
    ), 
    regexp = "data argument must refer to a data.frame"
  )
  expect_error(
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample", 
      sampling_weight = "w_sample", strata = "strata"
    )),
    regexp = NA
  )
  expect_error(
    suppressWarnings(everest(
      data = tibble::as.tibble(ict_sample), 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample", 
      sampling_weight = "w_sample", strata = "strata"
    )),
    regexp = NA
  )
  expect_error(
    suppressWarnings(everest(
      data = data.table::as.data.table(ict_sample), 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata"
    )),
    regexp = NA
  )
  expect_error(
    everest(
      data = ict_sample, 
      id = "blabla", dissemination_dummy = "dissemination", dissemination_weight = "w_calib", 
      sampling_weight = c("blabla", "bloblo")
    ),
    regexp = "The following arguments do not refer to a variable name"
  )
  expect_error(
    everest(
      data = ict_sample, 
      id = "blabla", dissemination_dummy = "dissemination", dissemination_weight = "w_calib", 
      sampling_weight = "blabla", 
      calibration_weight = "blabla", calibration_var = 2
    ), 
    regexp = "The following arguments do not refer to a vector of variable names"
  )
  expect_error(
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata",
      nrc_weight = "w_nrc", response_dummy = "blabla"
    ),
    regexp = "Some variables do not exist in ict_sample: \n  - response_dummy argument: blabla"
  )
})

test_that("argument value controls work as expected", {
  
  # id
  ict_sample$firm_id[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "contain any missing \\(NA\\) values.")
  rm(ict_sample)
  ict_sample$firm_id[1] <- ict_sample$firm_id[2]
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "contain any duplicated values.")
  rm(ict_sample)
  
  # dissemination_dummy
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "division", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "should be of type logical or numeric.")
  expect_error({
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    ))
  }, regexp = NA)
  ict_sample$dissemination[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)

  # dissemination_weight
  ict_sample$w_sample <- as.character(ict_sample$w_calib)
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)
  ict_sample$w_sample[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "contain any missing \\(NA\\) values.")
  rm(ict_sample)
  

  # sampling_weight
  ict_sample$w_sample <- as.character(ict_sample$w_sample)
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)
  ict_sample$w_sample[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample"
    )
  }, regexp = "contain any missing \\(NA\\) values.")
  rm(ict_sample)

  # strata
  ict_sample$strata <- suppressWarnings(as.numeric(ict_sample$strata))
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata"
    )
  }, regexp = " should be of type factor or character.")
  rm(ict_sample)
  expect_error({
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata"
    ))
  }, regexp = NA)
  ict_sample$strata[1] <- NA
  expect_error({
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata"
    ))
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # scope_dummy
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "division"
    )
  }, regexp = "should be of type logical or numeric.")
  expect_error({
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope"
    ))
  }, regexp = NA)
  ict_sample$scope[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_sample",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  ict_sample$scope[match(TRUE, ict_sample$resp)] <- FALSE
  expect_error({
    variance_wrapper <- everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    )
  }, regexp =  "The following units are out-of-scope")
  rm(ict_sample)
  
  # nrc_dummy
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "division"
    )
  }, regexp = "should be of type logical or numeric.")
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    )
  }, regexp = NA)
  ict_sample$nrc[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # response_dummy
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "division", nrc_dummy = "nrc"
    )
  }, regexp = "should be of type logical or numeric.")
  expect_error({
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    ))
  }, regexp = NA)
  ict_sample$resp[1] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # nrc_weight
  ict_sample$w_nrc <- as.character(ict_sample$w_nrc)
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)
  ict_sample$w_nrc2 <- ict_sample$w_nrc
  ict_sample$w_nrc2[match(TRUE, ict_sample$resp)] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc2", response_dummy = "resp", nrc_dummy = "nrc"
    )
  }, regexp = "should not contain any missing \\(NA\\) values for responding units.")
  rm(ict_sample)
  ict_sample$w_nrc[match(FALSE, ict_sample$resp)] <- NA
  expect_error({
    suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    ))
  }, regexp = NA)
  rm(ict_sample)
  
  # calibration_dummy
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_dummy = "division", calibration_var =  c("N_58", "N_59")
    )
  }, regexp = "should be of type logical or numeric.")
  ict_sample$calib <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_dummy = "calib", calibration_var =  c("N_58", "N_59")
    )
  }, regexp = "should not contain any missing \\(NA\\) values.")
  rm(ict_sample)
  
  # calibration_weight
  ict_sample$w_calib <- as.character(ict_sample$w_calib)
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_dummy = "calib", calibration_var =  c("N_58", "N_59")
    )
  }, regexp = "should be numeric.")
  rm(ict_sample)

  # calibration_var
  ict_sample$complex <- complex(real = 1:NROW(ict_sample), imaginary = 1:NROW(ict_sample))
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  "complex"
    )
  }, regexp = "The following calibration variables are neither quantitative")
  rm(ict_sample)
  ict_sample[match(TRUE, ict_sample$calib), c("N_58", "N_59")] <- NA
  expect_error({
    everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("N_58", "N_59")
    )
  }, regexp = "contain missing \\(NA\\) values for units used in the calibration process:")
  rm(ict_sample)
  expect_error({
    variance_wrapper <- suppressWarnings(everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  "division"
    ))
    variance_wrapper(ict_survey, turnover)
  }, regexp = NA)

})

test_that("methodological validation works as expected", {
  
  expect_error({
    variance_wrapper <- everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_nrc",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("N_58", "N_59")
    )
    
  }, regexp =  "The following units have a disseminated weight")

  ict_sample$dissemination[match(TRUE, ict_sample$resp)] <- FALSE
  ict_sample$scope[match(TRUE, ict_sample$resp)] <- FALSE
  expect_error({
    variance_wrapper <- everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc"
    )
    
  }, regexp =  "the following units are classified both as out-of-scope units")
  rm(ict_sample)
  
  ict_sample$strata[1:26] <- letters
  expect_warning({
    variance_wrapper <- everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("N_58", "N_59")
    )
    variance_wrapper(ict_survey, speed_quanti)
  }, regexp = "The following strata contain less than two sampled units.")
  rm(ict_sample)
  
  ict_sample$w_sample[1] <- ict_sample$w_sample[1] / 2
  expect_warning({
    variance_wrapper <- everest(
      data = ict_sample, 
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("N_58", "N_59")
    )
    variance_wrapper(ict_survey, speed_quanti)
  }, regexp = "The following strata contain units whose sampling weights")
  rm(ict_sample)
  
})

# TODO: Add more tests with out-of-scope units


test_that("everest works", {
  expect_error(
    everest(
      ict_sample, mean(turnover),
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("division", "turnover_58", "turnover_59")
    ), 
    regexp = NA
  )
  expect_error({
    everest_wrapper <- suppressWarnings(everest(
      ict_sample, define = TRUE,
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("division", "turnover_58", "turnover_59")
    ))
    everest_wrapper(ict_survey, mean(turnover))
  }, regexp = NA)
  expect_identical(
    suppressWarnings(everest(
      ict_sample, mean(turnover),
      id = "firm_id", dissemination_dummy = "dissemination", dissemination_weight = "w_calib",
      sampling_weight = "w_sample", strata = "strata", 
      scope_dummy = "scope",
      nrc_weight = "w_nrc", response_dummy = "resp", nrc_dummy = "nrc",
      calibration_weight = "w_calib", calibration_var =  c("division", "turnover_58", "turnover_59")
    )), 
    everest_wrapper(ict_survey, mean(turnover))
  )
})
  
