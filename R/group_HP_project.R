#' Produces forecasts using the Hamilton-Perry Method and optionally smoothing
#'
#' @description Produces 5 year forecasts out to specified year for multiple
#'
#' @param DF data frame of 18 age groups and 2 sexs for any number of groups for
#' any number of years of data
#' @param smooth use Inoue smoothing for groups
#' @param years_out int how many years out to forecast from proj_year
#' @param cwr_df df precalculated CWR values
#' @param ccr_df df precalculated CCR values
#' @param par_year year to build parameters from
#' @param proj_year year to start projections from
#' @return data frame object with projected values

group_HP_project <- function(
    DF, smooth = TRUE, years_out = 30, cwr_df = NULL, ccr_df = NULL,
    par_year = NULL, proj_year = NULL){

    if(is.null(par_year)){
        par_year <- max(DF$Year)
    }

    if(is.null(proj_year)){
        proj_year <- par_year
    }

    if(is.null(ccr_df)){
        ccr_df <- group_CCR_estimates(
            DF[(Year == par_year) | Year == (par_year - 5),], smooth = smooth)
    }

    if(is.null(cwr_df)){
        cwr_df <- group_CWR_estimates(DF[(Year == par_year) ,], smooth = smooth)
    }

    copy(HP_project(
        df_pre = NULL, DF[(Year == proj_year) ,], years_out, cwr_df, ccr_df))
}
