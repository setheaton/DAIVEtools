#' Value Summary Dataframe
#'
#' A synthetic dataset containing value summaries for intervention alternatives
#' estimated from a 5 component full factorial optimization trial.
#'
#' @format A data frame with 32 rows and 13 variables:
#' \describe{
#'   \item{A}{An integer indicating whether component A is on or off for each
#'   row}
#'   \item{B}{An integer indicating whether component B is on or off for each
#'   row}
#'   \item{C}{An integer indicating whether component C is on or off for each
#'   row}
#'   \item{D}{An integer indicating whether component D is on or off for each
#'   row}
#'   \item{E}{An integer indicating whether component E is on or off for each
#'   row}
#'   \item{names}{Alternative labels as char values}
#'   \item{Y1}{Outcome 1 as unscaled numeric values}
#'   \item{Y1.scale}{Outcome 1 scaled from 0 to 1}
#'   \item{Y2}{Outcome 2 as unscaled numeric values}
#'   \item{Y2.scale}{Outcome 2 scaled from 0 to 1}
#'   \item{Y3}{Outcome 3 as unscaled numeric values}
#'   \item{Y3.scale}{Outcome 3 scaled from 0 to 1}
#'   \item{cost}{Dollar cost for each row, as an integer.}
#' }
#' @source Synthetic data generated for demonstration purposes.
"value_summary"

"gpaDraws"
"actDraws"
