#' Check if df has all necessary components
#'
#' @description checks columns of df for use with HP functions
#'
#' @param DF data frame like object
#' @return None

validate_age_df <- function(DF){
    if(!is.data.frame(DF)){
        stop("Object is not a valid data frame.")
    }

    if(!all(c("Sex", "Age5", "value", "Year") %in% names(DF))){
        stop("Missing either Sex, Age5, Year, or value column in data frame")
    }

    if(length(levels(DF$Age5)) != 18){
        stop("DF must be a dataframe that has an Age5 variable with 18 levels")
    }
}
