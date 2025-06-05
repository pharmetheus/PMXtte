#' Dummy "observed" Time-To-Event data
#'
#' @format ## `simplettedata`
#' A data frame with 50 rows and 5 columns:
#' \describe{
#'   \item{ID}{a numeric, the subject identifier (from 0 to 50)}
#'   \item{TIME}{a numeric, the time of the observation (from 0 to 100, rounded to closest integer)}
#'   \item{DV}{a numeric, the dependent variable, 0 for censoring, 1 for event}
#'   \item{SEXF}{a factor, with two levels: "Male" and "Female"}
#'   \item{AUCQF}{a factor, with three levels: "1st", "2nd" and "3rd"}
#'   ...
#' }
"simplettedata"

#' Dummy "simulated" Time-To-Event data
#'
#' @format ## `simplettesimdata`
#' A data frame with 5000 rows and 6 columns. The "observed" data
#' (`PMXtte::simplettedata`) was replicated 100-fold, with new simulation times
#' between 0 and 115. Censoring was applied in simulations (DV = 0) if the
#' simulated time was over 100 or if the simulated time was greater than the
#' time of censoring in the "observed" data, whichever applied first.
#' \describe{
#'   \item{ITER}{a numeric, from 0 to 100}
#'   \item{ID}{a numeric, the subject identifier (from 0 to 50)}
#'   \item{TIME}{a numeric, the time of the observation (from 0 to 100, rounded to 2 digits)}
#'   \item{DV}{a numeric, the dependent variable, 0 for censoring, 1 for event}
#'   \item{SEXF}{a factor, with two levels: "Male" and "Female"}
#'   \item{AUCQF}{a factor, with three levels: "1st", "2nd" and "3rd"}
#'   ...
#' }
"simplettesimdata"
