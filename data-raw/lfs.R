rm(list = ls(all.names = TRUE))

set.seed(1)
library(data.table)
library(sampling)

# Parameters
n_area <- 4
n_dwel_area <- 20
N_area <- 80
size_area <- round(rnorm(N_area, 120, 2))
N_dwel <- sum(size_area)
size_dwel <- round(rgamma(N_dwel, shape = 1.1)) + 1
size_dwel[size_dwel >= 6] <- 6
N_ind <- sum(size_dwel)

# Generate individual data
id_ind <- paste0("I", formatC(1:N_ind, width = 5, flag = "0"))
unemp <- as.logical(rbinom(N_ind, 1, 0.05))
income <- rep(NA, N_ind)
income[unemp] <- pmax(rnorm(sum(unemp), 8, 4), 0)
income[!unemp] <- rgamma(sum(!unemp), shape = 3, scale = 7)
lfs_pop_ind <- data.table(id_ind, unemp, income)

# Aggregate by dwelling
lfs_pop_ind <- lfs_pop_ind[order(lfs_pop_ind$income + rnorm(N_ind, 0, 20)), ]
lfs_pop_ind$id_dwel <- paste0("D", formatC(
  unlist(lapply(seq_along(size_dwel), function(i) rep(i, size_dwel[i])), use.names = FALSE), 
width = 5, flag = "0"))
lfs_pop_dwel <- data.table(lfs_pop_ind)[, list(unemp = sum(unemp), income = sum(income)), by = id_dwel]
summary(lfs_pop_dwel$income)

# Aggregate by area
lfs_pop_dwel <- lfs_pop_dwel[order(lfs_pop_dwel$income + rnorm(N_dwel, 0, 20)), ]
lfs_pop_dwel$id_area <- paste0("A", formatC(
  unlist(lapply(seq_along(size_area), function(i) rep(i, size_area[i])), use.names = FALSE), 
width = 3, flag = "0"))
lfs_pop_area <- data.table(lfs_pop_dwel)[, list(unemp = sum(unemp), income = sum(income), number_dwelling = .N), by = id_area]
summary(lfs_pop_area$income)

# Draw the sample of areas (balanced sampling)
lfs_pop_area$pik_area <- lfs_pop_area$number_dwelling * n_area / sum(lfs_pop_area$number_dwelling)
lfs_pop_area$s <- as.logical(sampling::samplecube(
  X = as.matrix(lfs_pop_area[, c("pik_area", "number_dwelling", "income"), with = FALSE]), 
  pik = lfs_pop_area$pik_area
))

# Draw the sample of dwellings within the sampled areas (simple random sampling)
lfs_pop_dwel_samp_area <- lfs_pop_dwel[lfs_pop_dwel$id_area %in% lfs_pop_area$id_area[lfs_pop_area$s], ]
lfs_pop_dwel_samp_area <- 
  merge(lfs_pop_dwel_samp_area, lfs_pop_area[, c("id_area", "pik_area"), with = FALSE], by = "id_area")
lfs_pop_dwel_samp_area[, pik_dwel := 20 / .N, by = id_area]
lfs_pop_dwel_samp_area$pik <- lfs_pop_dwel_samp_area$pik_area * lfs_pop_dwel_samp_area$pik_dwel
lfs_pop_dwel_samp_area[, s := as.logical(srswor(20, .N)), by = id_area]

# Determine the individuals within the sampled dwellings (no sampling)
lfs_pop_ind_samp_dwel <- lfs_pop_ind[lfs_pop_ind$id_dwel %in% lfs_pop_dwel_samp_area$id_dwel[lfs_pop_dwel_samp_area$s], ]
lfs_pop_ind_samp_dwel <- 
  merge(lfs_pop_ind_samp_dwel, lfs_pop_dwel_samp_area[, c("id_dwel", "pik"), with = FALSE], by = "id_dwel")
lfs_pop_ind_samp_dwel$sampling_weight <- 1 / lfs_pop_ind_samp_dwel$pik


# Produce the final files
lfs_samp_area <- data.frame(lfs_pop_area, stringsAsFactors = FALSE)[
  lfs_pop_area$s, c("id_area", "number_dwelling", "income", "pik_area")
]
row.names(lfs_samp_area) <- NULL
lfs_samp_dwel <- data.frame(lfs_pop_dwel_samp_area, stringsAsFactors = FALSE)[
  lfs_pop_dwel_samp_area$s, c("id_dwel", "id_area", "income", "pik_area", "pik_dwel", "pik")
]
lfs_samp_dwel <- lfs_samp_dwel[order(lfs_samp_dwel$id_dwel), ]
row.names(lfs_samp_dwel) <- NULL
lfs_samp_ind <- data.frame(lfs_pop_ind_samp_dwel, stringsAsFactors = FALSE)[, 
  c("id_ind", "id_dwel", "income", "unemp", "sampling_weight")
]
lfs_samp_ind <- lfs_samp_ind[order(lfs_samp_ind$id_ind), ]
row.names(lfs_samp_ind) <- NULL

# Export the final files
save(lfs_samp_area, file = "data/lfs_samp_area.RData")
save(lfs_samp_dwel, file = "data/lfs_samp_dwel.RData")
save(lfs_samp_ind, file = "data/lfs_samp_ind.RData")
rm(list = ls(all.names = TRUE))
