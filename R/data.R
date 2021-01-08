#' King County population estimates
#'
#' A data set containing population estimates of King County, Washington census
#' tracts by age, race, ethnicity, and sex. Pulled from the Office of Finance
#' and Management.
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
#' @source \url{http://www.diamondse.info/}
"kc_pop_data"
