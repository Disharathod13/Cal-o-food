#' The Shiny App
#'
#' @description
#' Praises you or someone with a random word.
#' This function launches the Shiny app called 'Cal-o-food,' where users can select
#' their health goals using nutrient information from a wide array of variables
#' in the dataset. It allows users to choose from a range of trending fast food
#' restaurants and provides a list of recommendations tailored to their selection.
#' The recommendations include food items that best align with the user's preferences.
#' The results are visualized, providing a customized output based on the user's input.
#'
#'
#' @return Shiny App
#'
#' @export
run_app <- function() {
  app_dir <- system.file("Cal-o-food-app", package = "Calofood")
  shiny::runApp(app_dir, display.mode = "normal")
}
