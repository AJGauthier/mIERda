#' Calculates the intra-individual variability (IRV) for all the rows of a given dataset
#'
#' Takes a dataframe or matrix of item responses and returns for each participant
#' the within-person standard deviation
#'
#' @param df a dataframe of matrix of data (e.g. survey responses)
#' @param na.rm a logical statement (TRUE or FALSE) indicating whether NA values should be removed before computations.
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#' @references
#' Dunn, A. M., Heggestad, E. D., Shanock, L. R., & Theilgard, N. (2018).
#' Intra-individual Response Variability as an Indicator of Insufficient Effort Responding:
#' Comparison to Other Indicators and Relationships with Individual Differences.
#' \emph{Journal of Business and Psychology, 33(1)}, 105-121. \doi{10.1007/s10869-016-9479-0}
#'
#' Marjanovic, Z., Holden, R., Struthers, W., Cribbie, R., & Greenglass, E. (2015).
#' The inter-item standard deviation (ISD): An index that discriminates between conscientious and random responders.
#' \emph{Personality and Individual Differences}, 84, 79-83. \doi{10.1016/j.paid.2014.08.021}
#'
#' @export compute_isd
#' @importFrom stats sd
#' @examples
#' df <- mierda_data
#' isd <- compute_isd(df, na.rm = TRUE)
#'
compute_isd <- function(df, na.rm = TRUE) {
  # Check if the input is a data frame or matrix
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input data must be a data frame or matrix")
  }

  # Apply the standard deviation function row-wise
  within_person_sd <- apply(df, 1, stats::sd, na.rm = na.rm)  # na.rm = TRUE handles missing data

  return(within_person_sd)  # Return the vector of standard deviations
}
