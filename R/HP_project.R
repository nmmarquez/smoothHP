#' Produces forecasts using the Hamilton-Perry Method
#'
#' @description Produces 5 year forecasts out to specified year
#'
#' @param df_pre data frame like object at time t
#' @param df_post data frame like object at time t+5
#' @param years_out int how many years out to forecast from t+5
#' @param cwr_df df precalculated CWR values
#' @param ccr_df df precalculated CCR values
#' @return data frame object with projected values

HP_project <- function(
    df_pre = NULL, df_post = NULL, years_out = 20, cwr_df = NULL, ccr_df = NULL){
    if((years_out %% 5) != 0){
        stop("Years out must be divisible by 5")
    }

    forecast_years <- seq(
        df_post$Year[1] + 5, df_post$Year[1] + years_out, by = 5)
    if(is.null(cwr_df)){
        cwr_df <- calc_CWR(df_post)
    }
    if(is.null(ccr_df)){
        ccr_df <- calc_CCR(df_pre, df_post)
    }

    final_df <- rbind(df_pre, df_post)

    for(y in forecast_years){
        sub_tp5_df <- merge(
            final_df[Year == (y - 5),], ccr_df, all.x = TRUE,
            by = names(ccr_df)[!(names(ccr_df) %in% c("CCR", "Age5_post"))])
        sub_tp5_df[, value := value * CCR]
        sub_tp5_df[, Age5 := Age5_post]
        sub_tp5_df[, Year := Year + 5]
        rvec <- names(sub_tp5_df)[names(sub_tp5_df) != "value"]
        sub_tp5_df <- sub_tp5_df[, list(value = sum(value)), by = rvec]
        sub_tp5_df[, CCR := NULL]
        sub_tp5_df[, Age5_post := NULL]
        sub_tp5_df[, Agen := as.numeric(Age5)]

        wrem <- c("Agen", "Sex", "value", "Age5", "Year")
        wvec <- names(sub_tp5_df)[!(names(sub_tp5_df) %in% wrem)]

        woman_count <- sub_tp5_df[
            Sex == "Female" & Agen >= 4 & Agen <= 9,
            list(WC= sum(value)), by = wvec]
        woman_count[,ps:=1]

        mrem <- c("CWR", "Sex", "value", "Age5", "Year", "Agen")
        baby_df <- merge(
            sub_tp5_df[Age5 == "5-9",], cwr_df,
            by = names(cwr_df)[!(names(cwr_df) %in% c("CWR"))])
        baby_df[, ps:=1]
        mvec <- names(baby_df)[!(names(baby_df) %in% mrem)]
        baby_df <- merge(baby_df, woman_count, by = mvec)
        baby_df[, Age5 := factor("0-4", levels(sub_tp5_df$Age5))]
        baby_df[, value := WC * CWR]
        baby_df[, ps := NULL]
        baby_df[, CWR := NULL]
        baby_df[, WC := NULL]

        tp5_df <- rbind(baby_df, sub_tp5_df)
        tp5_df[, Agen:=NULL]

        final_df <- rbind(final_df, tp5_df)[Year > max(df_post$Year),]
    }

    copy(final_df)
}
