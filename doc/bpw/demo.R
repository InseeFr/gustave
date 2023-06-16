#-----------------------------------------------------
# demo.R
#
# Live demonstration of the variance estimation
# program for EU-SILC produced by the gustave package
#
# EU-SILC Best Pratices Workshop
# 14-15 september 2017 - Prague
#
# Martin Chevalier (Insee)
#-----------------------------------------------------


# Load the standalone .RData file
load("silc_bpw/precisionSilc14.RData")

### Standard use

# Mean equivalised disposable income per household
precisionSilc(h, HX090)

# Share of households below a given (absolute) threshold
# (see below for relative threshold e.g. arpr)
precisionSilc(h, HX090 < 20000)

# Work intensity
precisionSilc(r, as.factor(RX050))

# Number of people with severe material deprivation
precisionSilc(r, total(RX060 == 1))

# Ratio of equivalised disposable income 
# per person in the household
precisionSilc(h, ratio(HX090, HX040))



### Domain estimation

# Merging h and d files in order to retrieve the region (DB040)
h <- merge(
  h, d[, c("DB030", "DB040")]
  , by.x = "HB030", by.y = "DB030", all.x = TRUE
)
h$DB040[h$DB040 == ""] <- NA

# Domain estimation with where (FR10 : Paris area)
precisionSilc(h, HX090, where = DB040 == "FR10")

# Domain estimation with by
precisionSilc(h, HX090, by = DB040)



### Complex linearizations using the vardpoor package

# Installation of the vardpoor package
# install.packages("vardpoor")

# Merging r and h to get the equivalised disposable income
# for each member of the household
r <- merge(
  r, h[, c("HB030", "HX090", "DB040")]
  , by.x = "RX030", by.y = "HB030", all.x = TRUE
)

# Gini coefficient
precisionSilc(r, gini(HX090))

# At-risk of poverty rate
precisionSilc(r, arpr(HX090))

# Note : for AROPE, no proper linearization available
# Taken as if the threshold of the at-risk of poverty rate were absolute
precisionSilc(r, mean(RX070 != "000"))