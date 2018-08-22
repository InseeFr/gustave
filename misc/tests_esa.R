###
# tests_esa.R
# 
# 06/07/2018
###

racine <- "X:/HAB-EEC-Methodes/Estimation Enquetes Menages/_Commun/Outils/"

# Si gustave est chargé via git
# devtools::load_all("U://gustave")
# system("git config --global user.name \"Martin Chevalier\"")
# system("git config --global user.email martin.chevalier@insee.fr")

# Si gustave a été installé manuellement
library(gustave)

# Chargement des données
esa <- haven::read_sas(
  data_file = paste(racine, "#archives/qvar_180625/DONNEES/qvar_esa_eap_2012.sas7bdat", sep = "/")
)

tmp <- make_block(esa$r310, by = esa$secteur_calage)
calvar <- paste0("r310_", attr(tmp, "colby"))
tmp <- as.matrix(tmp)
colnames(tmp) <- calvar
esa <- cbind(esa, tmp)

# Création d'un wrapper qvar pour l'ESA
qvar_esa <- qvar(
  data = esa, 
  id = "siren",
  dissemination_dummy = "rep",
  dissemination_weight = "poids_apres_calage",
  sampling_weight = "poids_avt_calage", 
  strata = "strate",
  scope = "champ",
  response_dummy = "rep",
  nrc_weight = "poids_apres_cnr",
  calibration_weight = "poids_apres_calage",
  calibration_dummy = "ind_calage",
  calibration_var = c("secteur_calage", calvar),
  define = TRUE
)

# Tests sur le fichier complet
qvar_esa(esa, mean(r003))

# Tests sur le seul fichier de répondants
esa_rep <- esa[esa$rep == 1, ]

qvar_esa(esa_rep, mean(r003))
qvar_esa(esa_rep, ratio(r217, r216))

var <- c("r003", "r310")
qvar_esa(esa_rep, mean(var))