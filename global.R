#' Calculate the individual error
#'
#' @param guess The individual guess
#' @param true_value The true value of the number of balls
calc_individual_error <- function(guess, true_value) {
  return(
    abs(guess - true_value)
  )
}

#' Calculate the mean individual error
#'
#' @param guesses A vector with all the guesses
#' @inheritParams calc_individual_error
calc_avg_individual_error <- function(guesses, true_value) {
  return(
    mean(calc_individual_error(guesses, true_value), na.rm = TRUE)
  )
}

#' Group guess
#'
#' @inheritParams calc_avg_individual_error
calc_group_guess <- function(guesses) {
  return(
    mean(guesses, na.rm = TRUE)
  )
}

#' Group error
#' 
#' @param guess The group's guess
#' @inheritParams individual_error
calc_group_error <- function(guess, true_value) {
  return(
    abs(guess - true_value)
  )
}

#' 
#' The crowd is wise if the average of the individual errors is less than
#' the error of the group estimate, i.e., the crowd's guess is more precise.
calc_wisdom <- function(ind, group) {
  return(
    ind - group
  )
}
