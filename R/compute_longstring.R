#' Calculates the longest string of repetitive responses in a specified dataset
#'
#' Takes a dataframe or matrix of item responses and returns for each participant
#' the lenght of the longest repetitive string of consecutive responses.
#'
#'
#' @param df a dataframe of matrix of data (e.g. survey responses)
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#' @references
#' Johnson, J. A. (2005). Ascertaining the validity of individual protocols
#' from web-based personality inventories. \emph{Journal of Research in Personality, 39}, 103-129. \doi{10.1016/j.jrp.2004.09.009}
#'
#' @export compute_longstring
#'
#' @examples
#' df <- mierda_data
#' ls <- compute_longstring(df)
#'
compute_longstring <- function(df) {
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input data must be a data frame or matrix")
  }
  all.na <- apply(df, 1, function(x) {all(is.na(x))})
  df <- as.matrix(df) # dummy proofing coerce to matrix
  # Apply row-wise calculation of the longstring indicator
  longstrings <- apply(df, 1, function(row_data) {
    max(rle(row_data)$lengths)
  })
  longstrings[all.na] <- NA # Ensures rows with all NA get an output of NA.
  return(longstrings)
}

