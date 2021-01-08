#' Group CWR calculation function
#'
#' @description Function for doing cwr estimates for multiple groups at the same
#' time. Has the ability to smooth and smooth either to the sum of all the
#' groups or to another set of estimates that are passed to ps_df.
#'
#' @param DF data frame of 18 age groups and 2 sexs for any number of groups for
#' a single year of data
#' @param smooth logical whether to do any smoothing at all
#' @param ps_df a parent data frame to smooth to which will merge on the group
#' cwr data frame and will match based on the matching columns
#' @param components logical return subgroup components for multistage smoothing
#' @return data frame of cwr estimates for each sex for each group

group_CWR_estimates <- function(
    DF, smooth = TRUE, ps_df = NULL, components = FALSE){

    calc_vars <- c("Age5", "Sex", "Year", "value")
    nc_vars <- names(DF)[!(names(DF) %in% calc_vars)]
    group_cwr_df <- calc_CWR(DF[Year == max(Year),], TRUE, nc_vars)
    group_cwr_df[, pi := denom]
    group_cwr_df[, qi := numer]

    if(smooth){
        group_cwr_df[, P := sum(denom), by=list(Sex)]
        group_cwr_df[, Q := sum(numer), by=list(Sex)]
        group_cwr_df[, iw := sqrt(pi) / (sqrt(pi) + sqrt(P))]
        group_cwr_df[, W := sqrt(P) / (sqrt(pi) + sqrt(P))]
        group_cwr_df[, Z := Q/P]

        if(!is.null(ps_df)){
            group_cwr_df[, P := NULL]
            group_cwr_df[, Z := NULL]
            group_cwr_df[, Q := NULL]
            group_cwr_df <- merge(group_cwr_df, ps_df)
            group_cwr_df[, iw := sqrt(pi) / (sqrt(pi) + sqrt(P))]
            group_cwr_df[, W := sqrt(P) / (sqrt(pi) + sqrt(P))]
        }

        group_cwr_df[, CWR := iw * (qi/pi) + W * (Z)]
        group_cwr_df[, CWR := ifelse(!is.finite(CWR), Z, CWR)]
        group_cwr_df[, P := NULL]
        group_cwr_df[, Z := NULL]
        group_cwr_df[, Q := NULL]
        group_cwr_df[, denom := NULL]
        group_cwr_df[, numer := NULL]
        group_cwr_df[, iw := NULL]
        group_cwr_df[, W := NULL]
    }

    else{
        group_cwr_df[, denom := NULL]
        group_cwr_df[, numer := NULL]
    }

    if(!components){
        if(smooth){
            group_cwr_df[, pi := NULL]
            group_cwr_df[, qi := NULL]
        }
    }

    else{
        group_cwr_df[, P := pi]
        group_cwr_df[, Q := qi]
        group_cwr_df[, pi := NULL]
        group_cwr_df[, qi := NULL]
    }

    copy(group_cwr_df)
}
