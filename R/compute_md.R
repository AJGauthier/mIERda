#' Calculates the Mahalanobis Distance for the participants within a dataset
#'
#' Takes a dataframe or matrix of item responses, and applies Mahalanobis Distance.
#' Returns the distance and associated p-value for each respondent.
#'    IF return_pvalues = FALSE, returns a vector containing the distance;
#'    IF return_pvalues = TRUE returns a list containing the distance and p-value.
#'
#' @param df is a dataframe or matrix containing respondents responses (e.g. survey responses).
#' @param return_pvalues a logical statement (TRUE or FALSE) indicating whether to return p-values alongside Mahalanobis distance
#' @param max_missing is a numeric value between 0 and 1, specifying the maximal proportion of missing values allowed for each variable.
#' @param na.rm shoud missing values be ignored.
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
#' df <- mierda_data
#' md <- compute_md(df, max_missing = .5, return_pvalues = FALSE, na.rm = T)
#'
compute_md <- function(df, max_missing = .5, return_pvalues = FALSE, na.rm = T) {
  # Dummy proofing
  max_missing <- abs(max_missing)
  # Validate max_missing is between 0 and 1
  if (max_missing < 0 || max_missing > 1) {
    stop("'max_missing' must be between 0 and 1.")
  }
  # Stop if not df or matrix
  if (!is.data.frame(df) && !is.matrix(df)) {
    stop("Input data must be a data frame or matrix")
  }
  # Handle Rows with all missing
  all.na <- apply(df, 1, function(x) {all(is.na(x))})
  if(any(all.na)) {
    warning("Some rows only contain missing values. These rows were excluded from the Mahalanobis distance computation")
  }

  # Dummy proofing pt.2
  df_clean <- as.matrix(df_clean)  # coerce to matrix for efficiency
  zero_var_cols <- which(apply(df_clean, 2, stats::var) == 0) #Find 0 variance columns
  if(length(zero_var_cols) > 0) df_clean <- df_clean[, -zero_var_cols] #Remove 0 variance columns

  is.positive.definite <- function(mat) {
    all(eigen(mat)$values > 0)
  }

  try({
    df <- df[, colMeans(is.na(df)) < max_missing] # Remove variables with more missing values than threshold
    Sx <- cov(df, use = "pairwise.complete.obs") # Calculate covariance matrix
    Sx.inv <- solve(Sx) # Invert the covariance matrix
    x <- scale(df, scale = FALSE) # Scale responses
    md <- t(apply(x, 1, function(x) colSums(x * Sx.inv, na.rm)))
    md <- rowSums(md*x, na.rm) # Compute MD
    # Overwrite values in md to NA for all-na participants
    md[all.na] <- NA

    if (return_pvalues) {
      return(list(md = md,
                  p_values = stats::pchisq(md, df = ncol(df), lower.tail = FALSE)))
    } else {
      return(md)
    }
  }, silent = TRUE)

  # If fails apply lambda to address invertability issues
  lambda <- 1e-5 # If fails, specify a lambda to address possible invertability issues
  try({
    Sx <- stats::cov(df, use = "pairwise") + lambda * diag(ncol(df))
    Sx.inv <- solve(Sx) # Invert the covariance matrix
    x <- scale(df, scale = FALSE) # Scale responses
    md <- t(apply(x, 1, function(xx) colSums(xx * Sx.inv, na.rm)))
    md <- rowSums(md*x, na.rm) # Compute MD
    # Overwrite values in md to NA for all-na participants
    md[all.na] <- NA

    if (return_pvalues) {
      return(list(md = md,
                  p_values = stats::pchisq(md, df = ncol(df), lower.tail = FALSE)))
    } else {
      return(md)
    }
  }, silent = TRUE)

  return(NULL)
}
