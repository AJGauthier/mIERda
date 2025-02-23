#' Computes the data-driven cutoff based on the mIERda scores
#'
#' The mIERda_cutoff function takes the vector of mIERda scores and returns a numeric value representing the optimal cutoff.
#'
#' @param score a vector containing respondents mierda score returned by the mierda_score() function
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#'
#' @export mIERda_cutoff
#' @aliases mierda_cutoff
#' @importFrom mIERda mIERda_score
#' @examples
#' x <- cbind(ls, md, ptc, isd, respRel, respCoh)
#' score <- mIERda_score(x)
#' cutoff <- mIERda_cutoff(score)
#'
mIERda_cutoff <- function(score) {
  thresholds <- seq(min(score), max(score), length.out = 100)
  sse_values <- sapply(thresholds, function(thresh) {
    group1 <- score[score <= thresh]
    group2 <- score[score > thresh]

    sse1 <- sum((group1 - mean(group1))^2)
    sse2 <- sum((group2 - mean(group2))^2)

    return(sse1 + sse2)
  })
  best_cutoff <- thresholds[which.min(sse_values)]
  return(best_cutoff)
}
