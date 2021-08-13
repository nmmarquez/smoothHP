#' Functions for calculating average accuracy of projections.
#'
#' @param obs numeric vector of true observed values
#' @param pred numeric vector of forecasted values
#' @param lambda power parameter for MAPE-R calculation
#' @return forecast accuracy metric
#'
#' @examples
#' x <- rpois(10, 3) + 1
#' y <- rpois(10, 3) + 1
#'
#' calc_MAPE(x,y)
#' calc_MALPE(x,y)
#' calc_MAPER(x,y)
#'
#' @export

calc_MAPE <- function(obs, pred){
    mean(abs(obs - pred) / obs)
}

#' @rdname calc_MAPE
#'
#' @export

calc_MALPE <- function(obs, pred){
    mean((obs - pred) / obs)
}

#' @rdname calc_MAPE
#'
#' @export

calc_MAPER <- function(obs, pred, lambda = .5){
    mean((abs(obs - pred) / obs)^lambda)^(1/lambda)
}
