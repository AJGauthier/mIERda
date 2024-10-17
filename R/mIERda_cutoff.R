#' Computes the data-driven cutoff based on the mIERda scores
#'
#' The mIERda_cutoff function takes the vector of mIERda scores and returns a numeric value representing the optimal cutoff.
#'
#' @param x a vector containung respondents mierda_score
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#'
#' @export mIERda_cutoff
#' @examples
#' cutoff <- mIERda_cutoff(mierdaScore)
#'
#'

mIERda_cutoff <- function(mierda_score) {
  thresholds <- seq(min(mierda_score), max(mierda_score), length.out = 100)
  sse_values <- sapply(thresholds, function(thresh) {
    group1 <- mierda_score[mierda_score <= thresh]
    group2 <- mierda_score[mierda_score > thresh]

    sse1 <- sum((group1 - mean(group1))^2)
    sse2 <- sum((group2 - mean(group2))^2)

    return(sse1 + sse2)
  })
  best_cutoff <- thresholds[which.min(sse_values)]
  return(best_cutoff)
}
