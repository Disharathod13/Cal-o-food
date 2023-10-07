#' Calculates Compound Interest
#'
#' @description
#' This function accepts these three inputs, computes the compound interest,
#' and provides the result. Additionally, it incorporates a mechanism for
#' error checking to guarantee that all inputs are positive values since
#' negative values or zeros would be illogical within the context of compound interest
#' computations.
#' Principal (numeric): The initial sum of money or investment,
#' referred to as the principal amount, used for computing compound interest.
#' It must be a positive numeric value, symbolizing the initial monetary amount.
#' Rate (numeric): The annual interest rate, expressed as a decimal,
#' governing the rate at which the principal amount increases over time due to interest.
#' It should also be a positive numeric value.
#' Time (numeric): The duration, in years, over which interest is computed.
#' This parameter signifies the period during which the principal amount accrues
#' interest. It must be a positive numeric value indicating the number of years.
#'
#' @param principal Initial investment amount (positive numeric value).
#' @param rate Annual interest rate as a decimal (positive numeric value).
#' @param time Duration in years for interest calculation (positive numeric value).
#'
#' @return The compunded interest amount of the input.
#'
#' @examples
#' comp_int(450000,6,8)
#'
#' @export
comp_int <- function(principal, rate, time) {

  if (is.na(principal) || is.na(rate) || is.na(time)){
    stop("Principal, rate, and time cannot be NA.")
} else if (principal <= 0 || rate <= 0 || time <= 0) {
    stop("Principal, rate, and time must be greater than zero.")
} else if (!is.numeric(principal) || !is.numeric(rate) || !is.numeric(time)) {
    stop("Principal, rate, and time must be numerical.")
  }

  compound_interest <- principal * (1 + rate/100)^time - principal
  return(compound_interest)
}
