


precalc_area <- varDT(
  y = NULL, 
  pik = lfs_samp_area$pik_area, 
  x = as.matrix(lfs_samp_area[c("pik_area", "income")]),
  id = lfs_samp_area$id_area
)
lfs_samp_dwel$q_area <- setNames(precalc_area$diago, precalc_area$id)[lfs_samp_dwel$id_area]

1 / lfs_samp_dwel$pik_area^2 - lfs_samp_dwel$q_area

# lfs_samp_dwel$q_area <- 

technical_data <- list(
  ind = lfs_samp_ind[c("id_ind", "id_dwel", "sampling_weight")],
  dwel = lfs_samp_dwel[c("id_dwel", "pik_dwel", "id_area", "pik_area", "q_area")],
  area = list(id_area = lfs_samp_dwel$id_area, precalc_area = precalc_area)
)

var_lfs <- function(y, ind, dwel, area){

  variance <- list()
  
  # Variance associated with the sampling of the dwellings
  y <- sum_by(y, ind$id_dwel)
  variance[["dwel"]] <- var_srs(
    y = y, pik = dwel$pik_dwel, strata = dwel$id_area, 
    w = (1 / dwel$pik_area^2 - dwel$q_area)
  )
  
  # Variance associated with the sampling of the areas
  y <- sum_by(y = y, by = dwel$id_area)
  variance[["area"]] <- varDT(y = y, precalc = area$precalc_area)
  
  colSums(do.call(rbind, variance))

}

y <- matrix(as.numeric(lfs_samp_ind$unemp), ncol = 1, dimnames = list(lfs_samp_ind$id_ind))

sqrt(with(technical_data, variance_function(y = y, ind = ind, dwel = dwel, area = area)))

sum(lfs_samp_ind$unemp * lfs_samp_ind$sampling_weight)

precision_lfs <- define_variance_wrapper(
  variance_function = var_lfs,
  technical_data = technical_data, 
  reference_id = technical_data$ind$id_ind,
  reference_weight = technical_data$ind$sampling_weight,
  default_id = "id_ind"
)
precision_lfs(lfs_samp_ind, mean(unemp))

