

#' Sampling frame of the Structural business statistics (SBS)
#' survey in the construction sector (NACE rev 2 F section)
#'
#' A (fake) dataset containing basic identification information and
#' auxiliary variables for the sampling of the Structural business
#' statistics (SBS) survey in the construction sector (NACE rev 2 F section).
#'
#' @format A data frame with 4950 observations and 4 variables:
#' \describe{
#'   \item{id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{label}{label of the economic sub-sector}
#'   \item{turnover}{firm turnover, in euros}
#' }
"sbs_pop"


#' Sample of the Structural business statistics (SBS)
#' survey in the construction sector (NACE rev 2 F section)
#'
#' A (fake) dataset containing sampling information about the sample 
#' of the Structural business statistics (SBS) survey in the construction 
#' sector (NACE rev 2 F section).
#'
#' @format A data frame with 308 observations and 8 variables:
#' \describe{
#'   \item{id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{label}{label of the economic sub-sector}
#'   \item{turnover}{firm turnover, in euros}
#'   \item{w_sample}{sampling weight}
#'   \item{resp}{boolean indicating whether the firm did respond to the survey or not}
#'   \item{hrg}{homogeneous response group used for the unit non-response correction}
#'   \item{w_nr}{weight after unit non-response correction}
#' }
"sbs_sample"

#' Survey data of the Structural business statistics (SBS)
#' survey in the construction sector (NACE rev 2 F section)
#'
#' A (fake) dataset containing calibration and survey variables of the respondents 
#' to the Structural business statistics (SBS) survey in the construction 
#' sector (NACE rev 2 F section).
#'
#' @format A data frame with 267 observations and 12 variables:
#' \describe{
#'   \item{id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{label}{label of the economic sub-sector}
#'   \item{turnover}{firm turnover, in euros}
#'   \item{investment}{firm investment, in euros}
#'   \item{N_41, N_42, N_43, turnover_41, turnover_42, turnover_43}{calibration variables (by economic sub-sector)}
#'   \item{w_calib}{calibrated weight}
#' }
"sbs_survey"
