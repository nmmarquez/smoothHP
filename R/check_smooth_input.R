#' A utility function to assess population data input
#'
#' A utility function to assess population data for use with the functions in
#' smoothHP.
#'
#' @param DF a data.table object
#'
#' @examples
#' check_smooth_input(kc_pop_data)
#'
#' @export

check_smooth_input <- function(DF){
    if(!is.data.table(DF)){
        warning("smoothHP functions require a data.table object")
    }

    req_cols <- c(
        Year = "Year", Sex = "Sex", `Five Year Age Groups` = "Age5",
        `Population Estimates` = "value")

    for(i in 1:length(req_cols)){
        if(!(req_cols[i] %in% names(DF))){
            warning(
                "Missing column ", req_cols[i], " which should represnt ",
                names(req_cols)[i])
        }
    }

    if(length(setdiff(names(DF), req_cols)) == 0){
        warning(
            "Data should have at least one column in addition to ",
            "the required columns.")
    }

    uni_yrs <- unique(DF$Year)
    if(length(uni_yrs) == 1){
        warning("Must have at least two years of data.")
    }

    if(!any(abs(apply(combn(uni_yrs, 2), 2, diff)) == 5)){
        warning("At least two years in data should be five years apart.")
    }

    if(!all(sort(unique(DF$Sex)) == c("Female", "Male"))){
        warning(
            "Sex column must be character with two and only two values ",
            "of Female and Male")
    }

    if(!is.numeric(DF$value)){
        warning("value column representing population should be numeric.")
    }

    if(!all(as.numeric(unique(DF$Age5)) == 1:18)){
        warning(
            "Age5 column should be factor with 18 levels or integers 1 ",
            "through 18 representing 5 year age groups from 0 to 85+")
    }
}
