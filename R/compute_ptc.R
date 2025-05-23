#' Calculates the person-total correlation in a specified dataset
#'
#' Takes a dataframe or matrix of responses and returns for each participant
#' the correlation between their responses and the mean responses observed at a sample-level.
#' Missing values are removed before computation
#'
#' @param df a dataframe of matrix of data (e.g. survey responses)
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#' @references
#' Karabatsos, G. (2003). Comparing the aberrant response detection performance of
#' thirty six person-fit statistics. \emph{Applied Measurement in Education, 16}, 277–298. \doi{10.1207/S15324818AME1604_2}
#' @export compute_ptc
#'
#' @examples
#' df <- mierda_data
#' ptc <- compute_ptc(df)
#'
#'
compute_ptc <- function(df) {
  # Stop if not df or matrix
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input data must be a data frame or matrix")
  }

  # Transpose the dataframe or matrix and ensure it is a dataframe.
  x_t <- as.data.frame(t(df))

  #Get number of variables
  ncol <- ncol(x_t)

  # Dummy proof
  ncol <- as.numeric(ncol)

  # Compute the total score for each person
  x_t$total_score <- rowMeans(x_t, na.rm = T)

  # Compute the Pearson correlation between each item and the total score
  correlations_pt <- sapply(
    x_t[ , 1:(ncol(x_t) - 1)],
    function(x) {
      tryCatch({
        cor(x, x_t$total_score, use = "pairwise.complete.obs")
      }, warning = function(w) {
        NA  # Return NA on warning
      })
    }
  )

  # Store a numeric vector
  correlations_pt <- as.numeric(correlations_pt)

  # Remove the variable names
  unname(correlations_pt)

  return(correlations_pt)
}
