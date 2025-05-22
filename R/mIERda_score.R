#' Computes the mIERda score
#'
#' The mIERda_score function takes a dataframe or matrix containing participants ISD, PTC, longtring,
#' response reliability and response coherence and generates a fair cut forest. A vector containing mIERda scores is returned.
#'
#' @param x a dataframe of matrix containing respondents values on multiple IERIs
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#' @references\enumerate{
#' \item Liu, Fei Tony, Kai Ming Ting, and Zhi-Hua Zhou. "Isolation forest." 2008 Eighth IEEE International Conference on Data Mining. IEEE, 2008.
#' \item Liu, Fei Tony, Kai Ming Ting, and Zhi-Hua Zhou. "Isolation-based anomaly detection." ACM Transactions on Knowledge Discovery from Data (TKDD) 6.1 (2012): 3.
#' \item Cortes, David. "Imputing missing values with unsupervised random trees." arXiv preprint arXiv:1911.06646 (2019).
#' \item Cortes, David. "Revisiting randomized choices in isolation forests." arXiv preprint arXiv:2110.13402 (2021).
#' \item Cortes, David. "Isolation forests: looking beyond tree depth." arXiv preprint arXiv:2111.11639 (2021).
#' }
#' @export mIERda_score
#' @aliases mierda_score
#' @importFrom isotree isolation.forest
#' @importFrom stats predict
#' @examples
#' x <- cbind(ls, md, ptc, isd, coh_rel, resp_rel)
#' score <- mIERda_score(x)
#'
mIERda_score <- function(x) {

  df_t <- as.data.frame(x)

  # Flag all-NA rows
  all.na <- apply(df_t, 1, function(y) all(is.na(y)))
  if (any(all.na)) {
    warning("Some rows only contain missing values. These rows were excluded from the computation")
  }

  # Generate cleaned dataset (exclude all-NA rows)
  df_t_clean <- df_t[!all.na, ]

  # Isolation Forest Model
  model_orig <- isotree::isolation.forest(
    df_t_clean,
    ndim = 2,
    sample_size = 32,
    ntrees = 100,
    max_depth = NULL,
    missing_action = "fail",
    scoring_metric = "depth",
    ntry = 1,
    coefs = "normal",
    prob_pick_pooled_gain = 1,
    weigh_by_kurtosis = F,
    nthreads = 1
  )

  # Get score
  pred_clean <- predict(model_orig, df_t_clean, type = "score")

  # Add NA to output - keep original length
  score <- rep(NA_real_, nrow(df_t))  # Create full-length score vector
  score[!all.na] <- pred_clean        # Fill in predictions where applicable

  score_mierda <- as.numeric(score) # Ensures returns numeric vector

  return(score_mierda)
}

mierda_score <- mIERda_score #Alias
