#' Calculates Child Woman Ratios within a single time period
#'
#' @description Calculates Child Woman Ratios within a single time period
#' by sex
#'
#' @param DF data frame like object
#' @param composite logical, return numerator and denominator separately
#' @param parent_cols character vector, other columns to maintain in group by
#' @return data frame object with CWR calculated by sex

calc_CWR <- function(DF, composite = FALSE, parent_cols = c()){
    validate_age_df(DF)

    if(length(unique(DF$Year)) != 1){
        if(nrow(DF) != 36){
            stop("Must have exactly 36 rows (18 ages x 2 sexes x 1 year).")
        }
    }

    gcols <- c("ps", parent_cols)

    DF_ <- copy(DF)
    DF_[, ps := 1]
    DF_[, Agen := as.numeric(Age5)]
    w_df <- DF_[
        Sex == "Female" & Agen >= 4 & Agen <= 9,
        list(denom = sum(value)),
        by = gcols]

    cwr_df <- merge(w_df, DF_[Age5 == "0-4",], by = gcols)
    cwr_df[, CWR := value / denom]
    cwr_df[, numer := value]
    cwr_df[, value := NULL]
    cwr_df[, Year := NULL]
    cwr_df[, Age5 := NULL]
    cwr_df[, Agen := NULL]
    cwr_df[, ps := NULL]

    if(!composite){
        cwr_df[, numer := NULL]
        cwr_df[, denom := NULL]
    }

    copy(cwr_df)
}
