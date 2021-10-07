#' Produces forecasts using the Hamilton-Perry Method and Inoue multi layer
#' smoothing
#'
#' @description Produces 5 year forecasts out to specified year for multiple
#'
#' @param DF data frame of 18 age groups and 2 sexs for any number of groups for
#' any number of years of data
#' @param years_out int how many years out to forecast from proj_year
#' @param par_year year to build parameters from
#' @param proj_year year to start projections from
#' @param stages character vector or list of character vectors which indicate
#' the various levels to make HP estimates. Smoothing occurs between each item
#' in the list/vector.
#' @return data frame object with projected values
#'
#' @export
#' @examples
#' multi_stage_group_HP_project(kc_pop_data, par_year = 2015)

multi_stage_group_HP_project <- function(
    DF, years_out = 30, par_year = NULL, proj_year = NULL,
    stages = c("County", "Race", "GEOID")){

    if(is.null(par_year)){
        par_year <- max(DF$Year)
    }

    if(is.null(proj_year)){
        proj_year <- par_year
    }

    group_ccr_df <- multi_stage_CCR_estimates(
        DF[Year == par_year | Year == (par_year - 5),], stages = stages)

    group_cwr_df <- multi_stage_CWR_estimates(
        DF[(Year == par_year),], stages = stages)

    group_df <- DF %>%
        group_by(across(all_of(c(unlist(stages), "Year", "Age5", "Sex")))) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        as.data.table()

    group_HP_project(
        group_df,
        years_out = years_out,
        cwr_df = group_cwr_df,
        ccr_df = group_ccr_df,
        proj_year = proj_year
    )
}
