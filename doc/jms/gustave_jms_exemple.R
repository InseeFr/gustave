rm(list = ls(all.names = TRUE))

# EEC
load("V:/EMPLOI-VALID/EEC2/PRECISION/precisionEec144.RData")
eec1 <- precisionEec(data = z, total(acteu %in% 2))
eec2 <- precisionEec(data = z, 
  ratio(acteu %in% 2, acteu %in% c(1, 2))
)
eec3 <- precisionEec(data = z, where = age >= 50,
  ratio(acteu %in% 2, acteu %in% c(1, 2))
)
eec4 <- precisionEec(data = z, by = reg,
  ratio(acteu %in% 2, acteu %in% c(1, 2))
)

# SRCV
load("X:/HAB-EEC-Methodes/Estimation Enquetes Menages/SRCV/variance/donnees/output/precisionSrcv15.RData")
r <- merge(r, h[, c("HB030", "HX090")], by.x = "RB040", by.y = "HB030", all.x = TRUE)
srcv1 <- precisionSrcv(r, arpr(HX090))
srcv2 <- precisionSrcv(r, gini(HX090))


# Export
save(
  eec1, eec2, eec3, eec4, srcv1, srcv2, 
  file = "X:/HAB-EEC-Methodes/Estimation Enquetes Menages/_Commun/Documentation/gustave_jms/gustave_jms_exemple.RData"
)

