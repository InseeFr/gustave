---
title: gustave
output: 
  html_document: 
    highlight: haddock
    keep_md: yes
---



Gustave (Gustave: a User-oriented Statistical Toolkit for Analytical Variance Estimation) is an R package that provides a toolkit for analytical variance estimation in survey sampling. Apart from the implementation of standard variance estimators (Sen-Yates-Grundy, Deville-Tillé), its main feature is to help the methodologist produce easy-to-use variance estimation "wrappers", where systematic operations (linearization, domain estimation) are handled in a consistent and transparent way for the end user.

## Install

As gustave is not yet available on CRAN, for now the simplest way to install it on your computer is to use `devtools::install_github()`:


```r
install.packages("devtools")
devtools::install_github("martinchevalier/gustave")
```

## Example

In this example, we define a variance estimation wrapper adapted to the example data inspired by the Information and communication technology (ICT) survey. The subset of the (simulated) ICT survey has the following features:

- stratified one-stage sampling design of 650 firms;
- 612 responding firms, non-response correction through reweighting in homogeneous response groups based on economic sub-sector and turnover;
- calibration on margins (number of firms and turnover broken down by economic sub-sector).

### Step 1: Define the variance *function*

In this context, the variance estimation *function* can be defined as follows:


```r
library(gustave)

# Calibration variables matrix
x <- as.matrix(ict_survey[
  order(ict_survey$firm_id),
  c(paste0("N_", 58:63), paste0("turnover_", 58:63))
])

# Definition of the variance function
variance_function <- function(y){
  
  # Calibration
  y <- rescal(y, x = x)
  
  # Non-response
  y <- add0(y, rownames = ict_sample$firm_id)
  var_nr <- var_pois(y, pik = ict_sample$response_prob_est, w = ict_sample$w_sample)

  # Sampling
  y <- y / ict_sample$response_prob_est
  var_sampling <- var_srs(y, pik = 1 / ict_sample$w_sample, strata = ict_sample$division)

  var_sampling + var_nr
  
}

# Test of the variance function
y <- as.matrix(ict_survey$speed_quanti)
rownames(y) <- ict_survey$firm_id
variance_function(y)
## [1] 80789975
```


### Step 2: Define the variance *wrapper*

The next step is the definition of a variance *wrapper*, which is easier to use than the variance function: 


```r
variance_wrapper <- define_variance_wrapper(
  variance_function = variance_function,
  reference_id = ict_survey$firm_id,
  default = list(id = "firm_id", weight = "w_calib"),
  objects_to_include = c("x", "ict_sample")
)
```

**Note** The objects `x` and `ict_sample` are embedded within the function `variance_wrapper()` (`variance_wrapper` is a [closure](http://adv-r.had.co.nz/Functional-programming.html#closures))

### Step 3: Features of the variance wrapper


```r
# Better display of results
variance_wrapper(ict_survey, speed_quanti)
##                      call   n      est variance      std       cv    lower  upper
## 1 total(y = speed_quanti) 612 257596.2 80789975 8988.324 3.489308 239979.4 275213

# Mean linearization
variance_wrapper(ict_survey, mean(speed_quanti))
##                     call   n     est variance      std       cv    lower    upper
## 1 mean(y = speed_quanti) 612 33.5849 1.373304 1.171881 3.489308 31.28806 35.88174
# Ratio linearization
variance_wrapper(ict_survey, ratio(turnover, employees))
##                                       call   n      est variance      std       cv    lower    upper
## 1 ratio(num = turnover, denom = employees) 612 273.3094 448.0464 21.16711 7.744741 231.8227 314.7962

# Discretization of qualitative variables
variance_wrapper(ict_survey, speed_quali)
##                     call                    mod   n       est  variance       std        cv     lower     upper
## 1 total(y = speed_quali)        Less than 2 Mbs 612  267.0918  2716.120  52.11641 19.512549  164.9455  369.2381
## 2 total(y = speed_quali)   Between 2 and 10 Mbs 612 2427.3708 16487.383 128.40320  5.289806 2175.7052 2679.0365
## 3 total(y = speed_quali)  Between 10 and 30 Mbs 612 2973.6097 17463.254 132.14861  4.444047 2714.6032 3232.6162
## 4 total(y = speed_quali) Between 30 and 100 Mbs 612 1028.2434  9187.207  95.84992  9.321715  840.3810 1216.1058
## 5 total(y = speed_quali)          Above 100 Mbs 612  973.6844  7239.452  85.08497  8.738455  806.9209 1140.4478
# On-the-fly recoding
variance_wrapper(ict_survey, speed_quali == "Between 2 and 10 Mbs")
##                                               call   n      est variance      std       cv    lower    upper
## 1 total(y = speed_quali == "Between 2 and 10 Mbs") 612 2427.371 16487.38 128.4032 5.289806 2175.705 2679.036

# 1-domain estimation
variance_wrapper(ict_survey, speed_quanti, where = division == "58")
##                                                call   n      est variance      std       cv   lower    upper
## 1 total(y = speed_quanti, where = division == "58") 195 60979.04 14258259 3776.011 6.192309 53578.2 68379.89
# Multiple domains estimation
variance_wrapper(ict_survey, speed_quanti, by = division)
##                                     call by   n        est   variance       std        cv      lower      upper
## 1 total(y = speed_quanti, by = division) 58 195  60979.045 14258259.2 3776.0110  6.192309  53578.199  68379.890
## 2 total(y = speed_quanti, by = division) 59  86  25130.672  4581073.2 2140.3442  8.516860  20935.674  29325.669
## 3 total(y = speed_quanti, by = division) 60  45   8440.647   443479.8  665.9428  7.889713   7135.423   9745.871
## 4 total(y = speed_quanti, by = division) 61  50  26436.765  3248822.7 1802.4491  6.817964  22904.029  29969.500
## 5 total(y = speed_quanti, by = division) 62 189 115614.870 51050095.6 7144.9350  6.179945 101611.055 129618.685
## 6 total(y = speed_quanti, by = division) 63  47  20994.178  7208244.8 2684.8175 12.788390  15732.033  26256.324

# Multiple variables at a time
variance_wrapper(ict_survey, speed_quanti, big_data)
##                      call   n         est     variance        std        cv       lower       upper
## 1 total(y = speed_quanti) 612 257596.1760 80789975.280 8988.32439  3.489308 239979.3839 275212.9681
## 2     total(y = big_data) 612    525.5133     5085.584   71.31328 13.570215    385.7418    665.2847
variance_wrapper(ict_survey, speed_quanti, mean(big_data))
##                      call   n          est     variance          std        cv        lower        upper
## 1 total(y = speed_quanti) 612 2.575962e+05 8.078998e+07 8.988324e+03  3.489308 2.399794e+05 2.752130e+05
## 2      mean(y = big_data) 612 6.851542e-02 8.644703e-05 9.297690e-03 13.570215 5.029228e-02 8.673856e-02
# Flexible syntax for where and by arguments
# (similar to the aes() function in ggplot2)
variance_wrapper(ict_survey, where = division == "58", 
 mean(speed_quanti), mean(big_data * 100)
)
##                                                 call   n       est variance      std        cv     lower     upper
## 1   mean(y = speed_quanti, where = division == "58") 195 34.066505 4.450004 2.109503  6.192309 29.931955 38.201056
## 2 mean(y = big_data * 100, where = division == "58") 195  6.710888 2.365297 1.537952 22.917268  3.696557  9.725219
variance_wrapper(ict_survey, where = division == "58", 
 mean(speed_quanti), mean(big_data * 100, where = division == "61")
)
##                                                 call   n      est  variance      std        cv    lower    upper
## 1   mean(y = speed_quanti, where = division == "58") 195 34.06651  4.450004 2.109503  6.192309 29.93195 38.20106
## 2 mean(y = big_data * 100, where = division == "61")  50 23.43656 19.410685 4.405756 18.798649 14.80143 32.07168
variance_wrapper(ict_survey, where = division == "58", 
 mean(speed_quanti), mean(big_data * 100, where = NULL)
)
##                                               call   n       est  variance      std        cv     lower     upper
## 1 mean(y = speed_quanti, where = division == "58") 195 34.066505 4.4500044 2.109503  6.192309 29.931955 38.201056
## 2                         mean(y = big_data * 100) 612  6.851542 0.8644703 0.929769 13.570215  5.029228  8.673856
```


