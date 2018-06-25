rm(list = ls(all.names = TRUE))

variance_wrapper_ict <- define_simple_wrapper(
  data = ict_sample, id = "firm_id",
  samp_weight = "w_sample", strata = "division",
  nrc_weight = "w_nrc", resp = "resp",
  calib_weight = "w_calib", calib_var =  c("division", "turnover_58", "turnover_59")
)

variance_wrapper_ict(ict_survey, test = speed_quanti)


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


microbenchmark::microbenchmark(
  direct = ,
  cache = 
)

