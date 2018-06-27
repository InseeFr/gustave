
racine <- "X:/HAB-EEC-Methodes/Estimation Enquetes Menages/_Commun/Outils/"
reimporter_donnees <- FALSE

esa <- data.frame(lapply(haven::read_sas(
  data_file = paste(racine, "#archives/everest_180625/DONNEES/everest_esa_eap_2012.sas7bdat", sep = "/")
), as.vector), stringsAsFactors = FALSE)



library(gustave)
everest_esa <- everest(
  data = esa, id = "siren", 
  samp_weight = "poids_avt_calage", 
  strata = "strate",
  scope = "champ",
  resp_dummy = "rep",
  nrc_weight = "poids_apres_cnr",
  calib_weight = "poids_apres_calage",
  calib_dummy = "ind_calage",
  calib_var = "secteur_calage",
  define = TRUE
)

esa_rep <- esa[esa$rep == 1, ]
everest_esa(esa_rep, mean(r003), mean(r310))

length(unique(esa_rep$secteur_calage))

tmp <- Matrix::sparse.model.matrix(
  ~ . - 1, 
  data = stats::model.frame(~ ., data = esa_rep[, c("secteur_calage"), drop = FALSE])
)

tmp <- gustave:::discretize_qualitative_var(esa_rep$secteur_calage)
precalc <- rescal(y = NULL, x = tmp, w = esa_rep$poids_apres_calage)





rescal(y = )

sum(tapply(esa$poids_avt_calage, esa$strate, sd) > 1e-4, na.rm = TRUE)

sum(is.na(tapply(esa$poids_avt_calage, esa$strate, sd) > 1e-4), na.rm = TRUE)

length(unique(esa$strate))