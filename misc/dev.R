

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y)), 
  reference_id = ict_survey$firm_id, 
  default = list(id = "firm_id", weight = "w_calib")
)
tmp <- variance_wrapper(ict_survey, "blabla" = mean(speed_quanti, by = division), display = FALSE)
str(tmp)

variance_wrapper(ict_survey, blabla = speed_quali, by = division)