
racine <- "X:/HAB-EEC-Methodes/Estimation Enquetes Menages/_Commun/Outils/"
reimporter_donnees <- FALSE

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd() 
devtools::load_all("U://gustave")

esa <- data.frame(lapply(haven::read_sas(
  data_file = paste(racine, "#archives/everest_180625/DONNEES/everest_esa_eap_2012.sas7bdat", sep = "/")
), as.vector), stringsAsFactors = FALSE)

tmp <- make_block(esa$r310, by = esa$secteur_calage)
calvar <- paste0("r310_", attr(tmp, "colby"))
tmp <- as.matrix(tmp)
colnames(tmp) <- calvar
esa <- cbind(esa, tmp)


system("git config --global user.name \"Martin Chevalier\"")
system("git config --global user.email martin.chevalier@insee.fr")
# system("git remote rename origin gitlab")


everest_esa <- everest(
  data = esa, 
  id = "siren", 
  samp_weight = "poids_avt_calage", 
  strata = "strate",
  scope = "champ",
  resp_dummy = "rep",
  nrc_weight = "poids_apres_cnr",
  calib_weight = "poids_apres_calage",
  calib_dummy = "ind_calage",
  calib_var = c("secteur_calage", calvar),
  define = TRUE
)

exclude <- (tapply(esa$strate, esa$strate, length) < 2)[esa$strate] | esa$poids_avt_calage == 0
strata <- esa$strate[!exclude]
pik <- (1 / esa$poids_avt_calage)[!exclude]
pik <- tapply(pik, strata, base::mean)[strata]
tmp <- var_srs(y = NULL, pik = pik, strata = strata)

everest_esa(esa, mean(r310))

esa_rep <- esa[esa$rep == 1, ]
system.time(
  everest_esa(esa_rep, mean(r310))
)
system.time(
  everest_esa(esa_rep, mean(r310), by = substr(ape_rep, 1, 2))  
)



str(environment(everest_esa)$technical_data$calib$precalc)

environment(everest_esa)$technical_data$calib$precalc$x

setdiff(
  environment(everest_esa)$technical_data$calib$id,
  rownames(spy)
)

sum(is.na(spy[[1]]$lin[[1]]))
sum(is.na(spy[[1]]$data$y))
sum(is.na(spy[[1]]$weight$weight))

sum(is.na(environment(everest_esa)$reference_weight))

setdiff(
  names(spy[[1]]$weight$weight),
  names(environment(everest_esa)$reference_weight)
)

setdiff(
  esa_rep$siren,
  names(environment(everest_esa)$reference_weight)
)


sum(!is.na(spy@x))

spy$var[calib_var_quali] <- 
  lapply(calib$var[calib_var_quali], discretize_qualitative_var)


str(spy)
sum(esa$ind_calage)


str(environment(everest_esa)$technical_data$calib)

str(environment(everest_esa)$technical_data$calib$id)

length(unique(esa_rep$secteur_calage))

tmp <- Matrix::sparse.model.matrix(
  ~ . - 1, 
  data = stats::model.frame(~ ., data = esa_rep[, c("secteur_calage"), drop = FALSE])
)

tmp <- gustave:::discretize_qualitative_var(esa_rep$secteur_calage[esa_rep$ind_calage == 1])
precalc <- rescal(y = NULL, x = tmp, w = esa_rep$poids_apres_calage)





rescal(y = )

sum(tapply(esa$poids_avt_calage, esa$strate, sd) > 1e-4, na.rm = TRUE)

sum(is.na(tapply(esa$poids_avt_calage, esa$strate, sd) > 1e-4), na.rm = TRUE)

length(unique(esa$strate))
