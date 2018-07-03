rm(list = ls(all.names = TRUE))

variance_wrapper_ict <- define_simple_wrapper(
  data = ict_sample, id = "firm_id",
  samp_weight = "w_sample", strata = "strata",
  nrc_weight = "w_nrc", resp = "resp",
  calib_weight = "w_calib", calib_var =  c("division", "turnover_58", "turnover_59")
)

variance_wrapper_ict(ict_survey, speed_quanti, by = "strata")
replace_variable_name_with_symbol(spy["by"], envir = globalenv())
variance_wrapper_ict(ict_survey, speed_quanti, by = division)

variance_wrapper_ict(ict_survey, mean(speed_quanti))
speed_quanti2 <- ict_survey$speed_quanti
variance_wrapper_ict(ict_survey, mean(speed_quant2))
var <- c("speed_quanti", "speed_quali")
variance_wrapper_ict(ict_survey, mean(var))
num <- c("turnover", "employees")
denom <- c("employees", "turnover")
variance_wrapper_ict(ict_survey, ratio(num, denom))


call_list, data, evaluation_envir

everest(ict_sample, mean(turnover),
        id = "firm_id", samp_weight = "w_sample", strata = "division",
        nrc_weight = "w_nrc", resp_dummy = "resp",
        calib_weight = "w_calib", calib_var =  c("division", "turnover_58", "turnover_59")
)

everest_ict <- everest(ict_sample, define = TRUE,
                       id = "firm_id", samp_weight = "w_sample", strata = "division",
                       nrc_weight = "w_nrc", resp_dummy = "resp",
                       calib_weight = "w_calib", calib_var =  c("division", "turnover_58", "turnover_59")
)

everest_ict(ict_survey, mean(turnover))
everest_ict(ict_survey)


