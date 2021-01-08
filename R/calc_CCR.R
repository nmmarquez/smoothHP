#' Calculates Cohort Change Ratios across two time periods 5 years apart
#'
#' @description Calculates Cohort Change Ratios across two time periods 5
#' years apart
#'
#' @param df_pre data frame like object at time t should have values for 2 sexes and 18 age groups
#' @param df_post data frame like object at time t+5 should have values for 2 sexes and 18 age groups
#' @param composite logical, return numerator and denominator separately
#' @param parent_cols character vector, other columns to maintain in group by
#' @return data frame object with CCR calculate by age and sex

calc_CCR <- function(df_pre, df_post, composite = FALSE, parent_cols = c()){
    lapply(list(df_pre, df_post), validate_age_df)
    if(length(unique(c(df_pre$Year, df_post$Year))) != 2){
        if((df_post$Year[1] - df_pre$Year[1]) != 5){
            stop("Year columns not properly organized.")
        }
    }

    lages <- levels(df_pre$Age5)
    df_pre_ <- copy(df_pre)
    df_post_ <- copy(df_post)
    gcols <- c("ps", parent_cols, "Sex", "Agen")

    df_pre_[,Agen := as.numeric(Age5) + 1]
    df_pre_[,ps := 1]
    df_pre_[,pre := value]
    df_pre_[,Agen := ifelse(Agen == 19, 18, Agen)]
    df_pre_sum <- df_pre_[,list(pre = sum(pre)), by = gcols]

    df_post_[,Agen := as.numeric(Age5)]
    df_post_[,ps := 1]
    df_post_[,post := value]
    df_post_[,value := NULL]
    df_post_sum <- df_post_[,list(post = sum(post)), by = gcols]

    ccr_df <- merge(
        df_pre_sum,
        df_post_sum,
        by = c(gcols)
    )

    ccr_df[, CCR := post / pre]
    ccr_df[, Age5 := factor(lages[Agen-1], lages)]
    ccr_df[, Age5_post := factor(lages[Agen], lages)]
    ccr_df[, denom := pre]
    ccr_df[, numer := post]
    old_ccr_df <- ccr_df[Age5 == "80-84",]
    old_ccr_df [,Age5 := Age5_post]

    ccr_df <- rbind(ccr_df, old_ccr_df)
    ccr_df[, pre := NULL]
    ccr_df[, post := NULL]
    ccr_df[, ps := NULL]
    ccr_df[, Agen := NULL]

    if(!composite){
        ccr_df[, denom := NULL]
        ccr_df[, numer := NULL]
    }

    copy(ccr_df)
}
