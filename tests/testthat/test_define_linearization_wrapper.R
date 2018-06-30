
context("define_linearization_wrapper")

total2 <- define_linearization_wrapper(
  linearization_function = function(y, w, w2){
    na <- is.na(y)
    y[na] <- 0
    total <- sum(y * weight)
    list(
      lin = list(y), 
      metadata = list(est = total, n = sum(!na))
    )
  }, 
  arg_type = list(data = "y" , weight = c("w", "w2")),
  arg_not_affected_by_domain = "w2"
)

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y)), 
  reference_id = ict_survey$firm_id,
  reference_weight = ict_survey$w_calib,
  default_id = "firm_id"
)

# variance_wrapper(ict_survey, speed_quali, by = division)
# variance_wrapper(ict_survey, total2(speed_quanti), where = division == "59", by = division)

# tmp <- t(sapply(lapply(spy, `[[`, "metadata"), `[`, c("by", "mod")))
# table(unlist(tmp[, "by"]))
# tmp2 <- cbind(tmp, tmp)

# new <- list()
# dim(new) <- c(4, 0)

