

#' Sampling frame of the Information and communication technologies (ICT)
#' survey in the information and communication sector (NACE rev 2 J section)
#'
#' A (simulated) dataset containing basic identification information and
#' auxiliary variables for the sampling of the Information and communication
#' technologies (ICT) survey in the information and communication sector 
#' (NACE rev 2 J section).
#'
#' @format A data frame with 7670 observations and 4 variables:
#' \describe{
#'   \item{firm_id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{employees}{number of employees}
#'   \item{turnover}{firm turnover, in thousand euros}
#' }
#' 
#' @seealso \code{\link{ict_sample}} \code{\link{ict_survey}}

"ict_pop"


#' Sample of the Information and communication technologies (ICT)
#' survey in the information and communication sector (NACE rev 2 J section)
#' 
#' A (simulated) dataset containing sampling information about the sample 
#' of the Information and communication technologies (ICT)
#' survey in the information and communication sector (NACE rev 2 J section)
#'
#' @format A data frame with 650 observations and 8 variables:
#' \describe{
#'   \item{firm_id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{employees}{number of employees}
#'   \item{turnover}{firm turnover, in euros}
#'   \item{w_sample}{sampling weight}
#'   \item{resp}{boolean indicating whether the firm did respond to the survey or not}
#'   \item{hrg}{homogeneous response group used for the unit non-response correction}
#'   \item{w_nr}{weight after unit non-response correction}
#' }
#' 
#' @seealso \code{\link{ict_pop}} \code{\link{ict_survey}}
"ict_sample"

#' Survey data of the Information and communication technologies (ICT)
#' survey in the information and communication sector (NACE rev 2 J section)
#'
#' A (simulated) dataset containing calibration and survey variables of the respondents 
#' to the Information and communication technologies (ICT)
#' survey in the information and communication sector (NACE rev 2 J section)
#'
#' @format A data frame with 612 observations and 25 variables:
#' \describe{
#'   \item{firm_id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{employees}{number of employees}
#'   \item{turnover}{firm turnover, in euros}
#'   \item{w_sample}{sampling weight}
#'   \item{w_nr}{weight after unit non-response correction}
#'   \item{N_58, N_59, N_60, N_61, N_62, N_63, turnover_58, turnover_59, turnover_60, turnover_61, turnover_62, turnover_63}{calibration variables (number of firms and turnover broken down by economic sub-sector)}
#'   \item{w_calib}{calibrated weight}
#'   \item{speed_quanti, speed_quanti_NA}{internet connection speed of the firm in Mbits, without or with missing values}
#'   \item{speed_quali, speed_quali_NA}{internet connection speed of the firm recoded in classes, without or with missing values}
#'   \item{big_data, big_data_NA}{use of big data analytics within the firm, without or with missing values}
#' }
#' 
#' @seealso \code{\link{ict_pop}} \code{\link{ict_sample}}

"ict_survey"
