

#' Sampling frame of the Information and communication technologies (ICT)
#' survey
#'
#' A (simulated) dataset containing basic identification information and
#' auxiliary variables for the sampling of the Information and communication
#' technologies (ICT) survey in the information and communication sector 
#' (NACE rev 2 J section).
#'
#' @format A data frame with 7670 observations and 5 variables:
#' \describe{
#'   \item{firm_id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{employees}{number of employees}
#'   \item{turnover}{firm turnover, in thousand euros}
#'   \item{strata}{stratification variable}
#' }
#' 
#' @seealso \code{\link{qvar}}, \code{\link{ict_sample}}, \code{\link{ict_survey}}

"ict_pop"


#' Sample of the Information and communication technologies (ICT)
#' survey
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
#'   \item{strata}{stratification variable}
#'   \item{w_sample}{sampling weight}
#'   \item{scope}{boolean indicating whether the firm did belong to the scope of the survey or not}
#'   \item{resp}{boolean indicating whether the firm did respond to the survey or not}
#'   \item{nrc}{boolean indicating whether the firm did take part in the non-response correction process or not}
#'   \item{hrg}{homogeneous response group used for the non-response correction}
#'   \item{response_prob_est}{response probability of the unit estimated using homogeneous response groups}
#'   \item{w_nrc}{weight after unit non-response correction}
#'   \item{calib}{boolean indicating whether the firm was integrated in the calibration process or not (\code{TRUE} for all responding units)}
#'   \item{N_58, N_59, N_60, N_61, N_62, N_63, turnover_58, turnover_59, turnover_60, turnover_61, turnover_62, turnover_63}{calibration variables (number of firms and turnover broken down by economic sub-sector)}
#'   \item{w_calib}{calibrated weight}
#'   \item{dissemination}{boolean indicating whether the unit appears in the dissemination file}
#' }
#' 
#' @seealso \code{\link{qvar}}, \code{\link{ict_pop}}, \code{\link{ict_survey}}
"ict_sample"

#' Survey data of the Information and communication technologies (ICT)
#' survey
#'
#' A (simulated) dataset containing calibration and survey variables of the respondents 
#' to the Information and communication technologies (ICT)
#' survey in the information and communication sector (NACE rev 2 J section)
#'
#' @format A data frame with 612 observations and 11 variables:
#' \describe{
#'   \item{firm_id}{identifier of the firm}
#'   \item{division}{identifier of the economic sub-sector}
#'   \item{employees}{number of employees}
#'   \item{turnover}{firm turnover, in euros}
#'   \item{w_calib}{calibrated weight}
#'   \item{speed_quanti, speed_quanti_NA}{internet connection speed of the firm in Mbps, without or with missing values}
#'   \item{speed_quali, speed_quali_NA}{internet connection speed of the firm recoded in classes, without or with missing values}
#'   \item{big_data, big_data_NA}{use of big data analytics within the firm, without or with missing values}
#' }
#' 
#' @seealso \code{\link{qvar}}, \code{\link{ict_pop}}, \code{\link{ict_sample}}

"ict_survey"



#' Sample of areas in the Labour force survey
#'
#' A (simulated) dataset containing information about 4 geographical 
#' areas (about 120 dwellings each) sampled for the labour force survey.
#'
#' @format A data frame with 4 observations and 3 variables:
#' \describe{
#'   \item{id_area}{identifier of the area}
#'   \item{income}{total annual income of the area in thousand euros (from income registry)}
#'   \item{pik_area}{first-order inclusion probability of the area (proportional to the number of dwellings in the area)}
#' }
#' 
#' @seealso \code{\link{define_variance_wrapper}}, \code{\link{lfs_samp_dwel}}, \code{\link{lfs_samp_ind}}

"lfs_samp_area"

#' Sample of dwellings in the Labour force survey
#'
#' A (simulated) dataset containing information about 80 dwellings
#' sampled for the Labour force survey (in the 4 areas described
#' in \code{\link{lfs_samp_area}}).
#'
#' @format A data frame with 80 observations and 6 variables:
#' \describe{
#'   \item{id_dwel}{identifier of the dwelling}
#'   \item{id_area}{identifier of the area}
#'   \item{income}{total annual income of the dwelling in thousand euros (from income registry)}
#'   \item{pik_area}{first-order inclusion probability of the area}
#'   \item{pik_dwel}{first-order inclusion probability of the dwelling within the area (20 dwelling sampled per area)}
#'   \item{pik}{first-order inclusion probability of the dwelling}
#' }
#' 
#' @seealso \code{\link{define_variance_wrapper}}, \code{\link{lfs_samp_area}}, \code{\link{lfs_samp_ind}}

"lfs_samp_dwel"

#' Sample of individuals in the Labour force survey
#'
#' A (simulated) dataset containing information about 157 individuals
#' sampled for the Labour force survey (all members of the 80 dwellings
#' described in \code{\link{lfs_samp_dwel}}). It also contains the 
#' unemployment status extracted from the survey file (no non-response).
#'
#' @format A data frame with 157 observations and 5 variables:
#' \describe{
#'   \item{id_ind}{identifier of the individual}
#'   \item{id_dwel}{identifier of the dwelling}
#'   \item{income}{total annual income of the individual in thousand euros (from income registry)}
#'   \item{unemp}{unemployment status}
#'   \item{sampling_weight}{sampling weight of the individual (inverse of the first-order inclusion probability of the dwelling)}
#' }
#' 
#' @seealso \code{\link{define_variance_wrapper}}, \code{\link{lfs_samp_area}}, \code{\link{lfs_samp_dwel}}

"lfs_samp_ind"