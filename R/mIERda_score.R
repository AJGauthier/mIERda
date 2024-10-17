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
#' @importFrom isotree isolation.forest
#' @importFrom isotree predict.isolation_forest
#'
#' @examples
#' mierdaScore <- mIERda_score (x)
#'
#'
mIERda_score <- function(df_mierda) {

  df_t <- as.data.frame(df_mierda)

  # Isolation Forest Model
  model_orig <- isotree::isolation.forest(
    df_t,
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

  mierda_score <- isotree::predict(model_orig,
                       df_t,
                       type="score")

  score_ier <- as.numeric(mierda_score)
  return(score_ier)
}
