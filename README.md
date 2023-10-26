gustave [![R-CMD-check](https://github.com/InseeFr/gustave/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InseeFr/gustave/actions/workflows/R-CMD-check.yaml) [![CRAN_Status](http://www.r-pkg.org/badges/version/gustave)](https://cran.r-project.org/package=gustave) [![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](https://github.com/SNStatComp/awesome-official-statistics-software) 
=======

Gustave (Gustave: a User-oriented Statistical Toolkit for Analytical Variance Estimation) is an R package that provides a **toolkit for analytical variance estimation in survey sampling**. 

Apart from the implementation of standard variance estimators (Sen-Yates-Grundy, Deville-Tillé), its main feature is to **help he methodologist produce easy-to-use variance estimation *wrappers***, where systematic operations (statistic linearization, domain estimation) are handled in a consistent and transparent way. 

The **ready-to-use variance estimation wrapper `qvar()`**, adapted for common cases (e.g. stratified simple random sampling, non-response correction through reweighting in homogeneous response groups, calibration), is also included. The core functions of the package (e.g. `define_variance_wrapper()`) are to be used for more complex cases.

## Install

gustave is available on CRAN and can therefore be installed with the `install.packages()` function:

```
install.packages("gustave")
```

However, if you wish to install the latest version of gustave, you can use `devtools::install_github()` to install it directly from the [github.com repository](https://github.com/martinchevalier/gustave):

```
install.packages("devtools")
devtools::install_github("martinchevalier/gustave")
```

## Example

In this example, we aim at estimating the variance of estimators computed using simulated data inspired from the Information and communication technology (ICT) survey. This survey has the following characteristics:

- stratified one-stage sampling design;
- non-response correction through reweighting in homogeneous response groups based on economic sub-sector and turnover;
- calibration on margins (number of firms and turnover broken down by economic sub-sector).

The ICT simulated data files are shipped with the gustave package:

```
library(gustave)
data(package = "gustave")
? ict_survey
```

### Methodological description of the survey

A variance estimation can be perform in a single call of `qvar()`:
```
qvar(

  # Sample file
  data = ict_sample,
  
  # Dissemination and identification information
  dissemination_dummy = "dissemination",
  dissemination_weight = "w_calib",
  id = "firm_id",
  
  # Scope
  scope_dummy = "scope",
  
  # Sampling design
  sampling_weight = "w_sample", 
  strata = "strata",
  
  # Non-response correction
  nrc_weight = "w_nrc", 
  response_dummy = "resp", 
  hrg = "hrg",
  
  # Calibration
  calibration_weight = "w_calib",
  calibration_var = c(paste0("N_", 58:63), paste0("turnover_", 58:63)),
  
  # Statistic(s) and variable(s) of interest
  mean(employees)
 
)
```

The survey methodology description is however cumbersome when several variance estimations are to be conducted. As it does not change from one estimation to another, it could be defined once and for all and then re-used for all variance estimations. `qvar()` allows for this by defining a so-called variance *wrapper*, that is an easy-to-use function where the variance estimation methodology for the given survey is implemented and all the technical data used to do so included.

```
# Definition of the variance estimation wrapper precision_ict
precision_ict <- qvar(

  # As before
  data = ict_sample,
  dissemination_dummy = "dissemination",
  dissemination_weight = "w_calib",
  id = "firm_id",
  scope_dummy = "scope",
  sampling_weight = "w_sample", 
  strata = "strata",
  nrc_weight = "w_nrc", 
  response_dummy = "resp", 
  hrg = "hrg",
  calibration_weight = "w_calib",
  calibration_var = c(paste0("N_", 58:63), paste0("turnover_", 58:63)),
  
  # Replacing the variables of interest by define = TRUE
  define = TRUE
  
)

# Use of the variance estimation wrapper
precision_ict(ict_sample, mean(employees))

# The variance estimation wrapper can also be used on the survey file
precision_ict(ict_survey, mean(speed_quanti))
```

### Features of the variance estimation wrapper

The variance estimation *wrapper* is much easier-to-use than a standard variance estimation function: 

- several statistics in one call (with optional labels): 

    ```
    precision_ict(ict_survey, 
      "Mean internet speed in Mbps" = mean(speed_quanti), 
      "Turnover per employee" = ratio(turnover, employees)
    )
    ```
    
- domain estimation with where and by arguments

    ```
    precision_ict(ict_survey, 
      mean(speed_quanti), 
      where = employees >= 50
    )
    precision_ict(ict_survey, 
      mean(speed_quanti), 
      by = division
    )
    
    # Domain may differ from one estimator to another
    precision_ict(ict_survey, 
      "Mean turnover, firms with 50 employees or more" = mean(turnover, where = employees >= 50),
      "Mean turnover, firms with 100 employees or more" = mean(turnover, where = employees >= 100)
    )
    ```

- handy variable evaluation

    ```
    # On-the-fly evaluation (e.g. discretization)
    precision_ict(ict_survey, mean(speed_quanti > 100))
    
    # Automatic discretization for qualitative (character or factor) variables
    precision_ict(ict_survey, mean(speed_quali))
    
    # Standard evaluation capabilities
    variables_of_interest <- c("speed_quanti", "speed_quali")
    precision_ict(ict_survey, mean(variables_of_interest))
    ```
    
- Integration with %>% and dplyr

    ```
    library(dplyr)
    ict_survey %>% 
      precision_ict("Internet speed above 100 Mbps" = mean(speed_quanti > 100)) %>% 
      select(label, est, lower, upper)
    ```

## Colophon

This software is an [R](https://cran.r-project.org/) package developed with the [RStudio IDE](https://www.posit.co/) and the [devtools](https://CRAN.R-project.org/package=devtools), [roxygen2](https://CRAN.R-project.org/package=roxygen2) and [testthat](https://CRAN.R-project.org/package=testthat) packages. Much help was found in [R packages](https://r-pkgs.org/) and [Advanced R](https://adv-r.hadley.nz/) both written by [Hadley Wickham](https://hadley.nz/).

From the methodological point of view, this package is related to the [Poulpe SAS macro (in French)](http://jms-insee.fr/jms1998_programme/#1513415199356-a8a1bdde-becd) developed at the French statistical institute. From the implementation point of view, some inspiration was found in the [ggplot2](https://CRAN.R-project.org/package=ggplot2) package. The idea of developing an R package on this specific topic was stimulated by the [icarus](https://CRAN.R-project.org/package=icarus) package and its author.
