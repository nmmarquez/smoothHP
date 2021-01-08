# helper function that allows you to use the calc_CCR function but not have to
# split up the two years used into different data frames.
split_CCR <- function(DF, parent_cols = c(), ...){
    calc_CCR(
        as.data.table(DF)[Year == min(Year),],
        as.data.table(DF)[Year == max(Year),], TRUE, parent_cols)
}

#' Group CCR calculation function
#'
#' @description Function for doing ccr estimates for multiple groups at the same
#' time. Has the ability to smooth and smooth either to the sum of all the
#' groups or to another set of estimates that are passed to ps_df.
#'
#' @param DF data frame of 18 age groups and 2 sexs for any number of groups for
#' 2 years of data which should be 5 years apart
#' @param smooth logical whether to do any smoothing at all
#' @param ps_df a parent data frame to smooth to which will merge on the group
#' ccr data frame and will match based on the matching columns
#' @param components logical return subgroup components for multistage smoothing
#' @return data frame of ccr estimates for each sex and age for each group

group_CCR_estimates <- function(
    DF, smooth = TRUE, ps_df = NULL, components = FALSE){

    calc_vars <- c("Age5", "Sex", "Year", "value")
    nc_vars <- names(DF)[!(names(DF) %in% calc_vars)]
    group_ccr_df <- split_CCR(DF, nc_vars)
    group_ccr_df[, pi := denom]
    group_ccr_df[, qi := numer]

    if(smooth){
        group_ccr_df[, P := sum(denom), by=list(Sex, Age5, Age5_post)]
        group_ccr_df[, Q := sum(numer), by=list(Sex, Age5, Age5_post)]
        group_ccr_df[, iw := sqrt(pi) / (sqrt(pi) + sqrt(P))]
        group_ccr_df[, W := sqrt(P) / (sqrt(pi) + sqrt(P))]
        group_ccr_df[, Z := Q/P]

        if(!is.null(ps_df)){
            group_ccr_df[, P := NULL]
            group_ccr_df[, Z := NULL]
            group_ccr_df[, Q := NULL]
            group_ccr_df <- merge(group_ccr_df, ps_df)
            group_ccr_df[, iw := sqrt(pi) / (sqrt(pi) + sqrt(P))]
            group_ccr_df[, W := sqrt(P) / (sqrt(pi) + sqrt(P))]
        }

        group_ccr_df[, CCR := iw * (qi/pi) + W * (Z)]
        group_ccr_df[, CCR := ifelse(!is.finite(CCR), Z, CCR)]
        group_ccr_df[, P := NULL]
        group_ccr_df[, Z := NULL]
        group_ccr_df[, Q := NULL]
        group_ccr_df[, denom := NULL]
        group_ccr_df[, numer := NULL]
        group_ccr_df[, iw := NULL]
        group_ccr_df[, W := NULL]
    }

    else{
        group_ccr_df[, denom := NULL]
        group_ccr_df[, numer := NULL]
    }

    if(!components){
        if(smooth){
            group_ccr_df[, pi := NULL]
            group_ccr_df[, qi := NULL]
        }
    }

    else{
        group_ccr_df[, P := pi]
        group_ccr_df[, Q := qi]
        group_ccr_df[, pi := NULL]
        group_ccr_df[, qi := NULL]
    }

    copy(group_ccr_df)
}
