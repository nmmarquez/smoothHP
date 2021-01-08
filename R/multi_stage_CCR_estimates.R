#' Group CCR calculation function
#'
#' @description Function for doing ccr estimates for multiple groups at the same
#' time. Will smooth using a multi stage smoothing process as outlined in Inoue
#' 2016.
#'
#' @param DF data frame of 18 age groups and 2 sexs for any number of groups for
#' a single year of data
#' @param stages character vector or list of character vectors which indicate
#' the various levels to make HP estimates. Smoothing occurs between each item
#' in the list/vector.
#' @return data frame of ccr estimates for each sex for each group
#' with an additional layer of smoothing
#'
#' @examples
#' multi_stage_CCR_estimates(kc_pop_data, stages = c("County", "Race"))
#'
#' @export
#' @import dplyr
#' @import data.table

multi_stage_CCR_estimates <- function(DF, stages = c("County", "Race", "GEOID")){

    top_df <- DF %>%
        group_by(across(all_of(c(
            unlist(stages[1]), "Year", "Age5", "Sex")))) %>%
        summarize(value = sum(value), .groups = "drop") %>%
        as.data.table()

    parent_ccr_df <-  group_CCR_estimates(
        top_df, smooth = FALSE, components = TRUE, ps_df = NULL) %>%
        dplyr::select(all_of(c(
            unlist(stages[1]), "Age5", "Sex", "CCR", "P", "Q", "Age5_post"))) %>%
        rename(Z = CCR) %>%
        as.data.table()

    if(length(stages) > 1){
        for(i in 2:length(stages)){
            staged_df <- DF %>%
                group_by(across(all_of(c(
                    unlist(stages[1:i]), "Year", "Age5", "Sex")))) %>%
                summarize(value = sum(value), .groups = "drop") %>%
                as.data.table()

            parent_ccr_df <- group_CCR_estimates(
                staged_df, smooth = TRUE, components = TRUE,
                ps_df = select(parent_ccr_df, -Age5_post)) %>%
                dplyr::select(all_of(c(
                    unlist(stages[1:i]), "Age5", "Sex", "CCR",
                    "P", "Q", "Age5_post"))) %>%
                rename(Z = CCR) %>%
                as.data.table()
        }
    }

    out_df <- parent_ccr_df %>%
        select(-P, -Q) %>%
        rename(CCR = Z) %>%
        as.data.table()

    return(copy(out_df))
}
