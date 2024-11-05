#' Calculates the Mahalanobis Distance for the participants within a dataset
#'
#' akes a dataframe or matrix of item responses, and applies Mahalanobis Distance.
#' Returns the distance and associated p-value for each respondent.
#'    IF return_pvalues = FALSE, returns a vector containing the distance;
#'    IF return_pvalues = TRUE returns a list containing the distance and p-value.
#'
#' @param df is a dataframe or matrix containing respondents responses (e.g. survey responses)
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#' @references
#' #' Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data.
#' \emph{Psychological Methods, 17(3)}, 437-455. \doi{10.1037/a0028085}
#'
#' @export compute_md
#' @importFrom stats mahalanobis
#' @importFrom stats cov
#' @importFrom stats var
#' @importFrom stats pchisq
#'
#' @examples
#' md <- compute_md(df, return_pvalues = FALSE)
#' md <- compute_md(df, return_pvalues = TRUE)
#'
compute_md <- function(df, return_pvalues = FALSE) {
  # Stop if not df or matrix
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input data must be a data frame or matrix")
  }

  df <- as.matrix(df)  # dummy proofing -- coerce to matrix for efficiency

  try({
    zero_var_cols <- which(apply(df, 2, stats::var) == 0)
    if(length(zero_var_cols) > 0) df <- df[, -zero_var_cols]
    md <- stats::mahalanobis(df, colMeans(df), cov(df))

    if (return_pvalues) {
      return(list(md = md, p_values = stats::pchisq(md, df = ncol(df), lower.tail = FALSE)))
    } else {
      return(md)
    }
  }, silent = TRUE)

  lambda <- 1e-4 #specify a lambda to address invertability issues
  try({
    cov_matrix <- stats::cov(df) + lambda * diag(ncol(df))
    md <- stats::mahalanobis(df, colMeans(df), cov_matrix)

    if (return_pvalues) {
      return(list(md = md, p_values = stats::pchisq(md, df = ncol(df), lower.tail = FALSE)))
    } else {
      return(md)
    }
  }, silent = TRUE)

  return(NULL)
}
