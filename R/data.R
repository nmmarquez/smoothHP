#' King County population estimates
#'
#' A data set containing population estimates of King County, Washington census
#' tracts by age, race, ethnicity, and sex. Pulled from the Office of Finance
#' and Management. The value column contains the population estimate for a
#' particular demographic group. These numbers are not integers and OFM states
#' the following: OFM leaves the data in decimals for two reasons. First,
#' we want the results to total to control numbers correctly and rounding would
#' make that impossible. Second, we want to remind users that these data are
#' estimates only and not actual enumerations.
#'
#' @format A data frame with 200,592 rows and 7 variables:
#' \describe{
#'   \item{GEOID}{Census tract fips code indicator}
#'   \item{Year}{year of the estimate}
#'   \item{Sex}{Male or Female}
#'   \item{Race}{Race and Ethnicity categories}
#'   \item{Age5}{5 year age categories}
#'   \item{value}{population estimate, not integer values}
#'   \item{County}{Integer for county indicator}
#' }
#' @source \url{https://ofm.wa.gov/washington-data-research/population-demographics/population-estimates/estimates-april-1-population-age-sex-race-and-hispanic-origin}
"kc_pop_data"
