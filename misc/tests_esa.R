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
esa <- data.frame(lapply(haven::read_sas(
  data_file = paste(racine, "#archives/everest_180625/DONNEES/everest_esa_eap_2012.sas7bdat", sep = "/")
), as.vector), stringsAsFactors = FALSE)

tmp <- make_block(esa$r310, by = esa$secteur_calage)
calvar <- paste0("r310_", attr(tmp, "colby"))
tmp <- as.matrix(tmp)
colnames(tmp) <- calvar
esa <- cbind(esa, tmp)

# Création d'un wrapper everest pour l'ESA
everest_esa <- everest(
  data = esa, 
  id = "siren",
  diss_dummy = "rep",
  diss_weight = "poids_apres_calage",
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

# Tests sur le fichier complet
everest_esa(esa, mean(r310))

# Tests sur le seul fichier de répondants
esa_rep <- esa[esa$rep == 1, ]

everest_esa(esa_rep, total("r003"))
everest_esa(esa_rep, ratio("r217", "r216"))

var <- c("r003", "r310")
everest_esa(esa_rep, mean(var))