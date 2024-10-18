#' Calculates screens and identifies C/IER in a specified dataset
#'
#' Turnkey function for IER screening. Takes a dataframe of item responses, a list specifying the scale names and
#' the items belonging to it, and a list indicating the number of latent factors for each scale. Computes the relevant C/IERIs,
#' computes the anomaly score, computes the relevant cutoff and returns a new dataframe (df_mierda) containing participants
#' classification (Attentive vs IER).
#'
#' @param df a dataframe of data (e.g. survey responses); a matrix won't work
#' @param scales_list a list of length number of scale indicating the name of the scales
#' included in the df and the items for each scale.
#' @param nb_factors a list of length number of scales indicating the name of each scale
#' and the number of latent factors for each scale included in the df
#'
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com }
#' @references\enumerate {
#' \item Liu, Fei Tony, Kai Ming Ting, and Zhi-Hua Zhou. "Isolation forest." 2008 Eighth IEEE International Conference on Data Mining. IEEE, 2008.
#' \item Liu, Fei Tony, Kai Ming Ting, and Zhi-Hua Zhou. "Isolation-based anomaly detection." ACM Transactions on Knowledge Discovery from Data (TKDD) 6.1 (2012): 3.
#' \item Cortes, David. "Imputing missing values with unsupervised random trees." arXiv preprint arXiv:1911.06646 (2019).
#' \item Cortes, David. "Revisiting randomized choices in isolation forests." arXiv preprint arXiv:2110.13402 (2021).
#' \item Cortes, David. "Isolation forests: looking beyond tree depth." arXiv preprint arXiv:2111.11639 (2021).
#' \item Karabatsos, G. (2003). Comparing the aberrant response detection performance of
#' thirty six person-fit statistics. \emph{Applied Measurement in Education, 16}, 277–298. \doi{10.1207/S15324818AME1604_2}
#' \item Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in survey data.
#' \emph{Psychological Methods, 17(3)}, 437-455. \doi{10.1037/a0028085}
#' \item Dunn, A. M., Heggestad, E. D., Shanock, L. R., & Theilgard, N. (2018).
#' Intra-individual Response Variability as an Indicator of Insufficient Effort Responding:
#' Comparison to Other Indicators and Relationships with Individual Differences.
#' \emph{Journal of Business and Psychology, 33(1)}, 105-121. \doi{10.1007/s10869-016-9479-0}
#' \item Marjanovic, Z., Holden, R., Struthers, W., Cribbie, R., & Greenglass, E. (2015).
#' The inter-item standard deviation (ISD): An index that discriminates between conscientious and random responders.
#' \emph{Personality and Individual Differences}, 84, 79-83. \doi{10.1016/j.paid.2014.08.021}
#' \item Johnson, J. A. (2005). Ascertaining the validity of individual protocols
#' from web-based personality inventories. \emph{Journal of Research in Personality, 39},103-129. \doi{10.1016/j.jrp.2004.09.009}
#' \item Dupuis, M., Meier, E., Capel, R., & Gendre, F. (2015). Measuring individuals’ response quality
#' in self-administered psychological tests: An introduction to Gendre’s functional method.
#' \emph{Frontiers in Psychology, 6}, 629. \doi{10.3389/FPSYG.2015.00629}
#' }
#'
#' @export remove_mIERda
#' @importFrom psych principal
#' @importFrom stats cor
#' @importFrom stats mahalanobis
#' @importFrom stats cov
#' @importFrom isotree isolation.forest
#' @importFrom stats predict
#'
#' @examples
#' df_screened <- remove_mIERda(df, scales_list, nb_factors)

remove_mIERda <- function(df, scales_list, nb_factors) {

  if (!is.data.frame(df)) {
    stop("Input data must be a data frame")
  }

  # Create new df to store results
  df_mierda <- data.frame(matrix(nrow = nrow(df), ncol = 0))

  # Compute longstring
  df_mierda$ls <- compute_longstring(df) #store in df df_mierda

  # Compute ISD
  df_mierda$isd <- compute_isd(df, na.rm = TRUE)

  # Compute MD
  df_mierda$md <- compute_md (df, return_pvalues = FALSE)

  # Compute PTC
  df_mierda$ptc <- compute_ptc(df)

  # Response reliability and coherence
  x <- compute_cohRel(df, scales_list, nb_factors)
  output <- as.data.frame(x)
  df_mierda <- cbind(df_mierda, output)

  # Compute_mierda score
  score <- mIERda_score(df_mierda)

  # Get cutoff
  mierda_cutoff <- mIERda_cutoff(score)

  # Classification
  df_mierda$mIERdaScore <- score

  # Class output
  df_mierda$classification <- ifelse(score <= mierda_cutoff, "attentive", "IER")

  # Join og and df with IER screening
  df_mierda <- cbind(df, df_mierda)

  return(df_mierda)
}
