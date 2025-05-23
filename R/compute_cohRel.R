#' Calculates the response coherence and reliability
#'
#' @description Takes a dataframe of item responses, a list specifying the scale names and
#' the items belonging to it, and a list indicating the number of latent factors for each scale.
#' The response reliability and coherence is then obtained by running a series of PCA, computing
#' the vector of response strategy and by computing the norm of the response strategy (response coherence)
#' and the correlation between two parallel halves  of the response strategy. The function returns a list
#' which contains 2 sub-list of length number of scales. Depending on the number of latent factors in your scale,
#' it might take from a few seconds to a few minutes for the code to run.
#'
#' @param df a dataframe of data (e.g. survey responses); a matrix won't work
#' @param scales_list a list of length number of scale indicating the name of the scales
#' included in the df and the items for each scale.
#' @param nb_factors a list of length number of scales indicating the name of each scale
#' and the number of latent factors for each scale included in the df
#' @param max_iterations a numerical value indicating the maximal number of iterations
#' allowed to obtain strict orthogonality and perfect communality, assuming
#' the model does not converge before reaching said number of iterations. This allows you to
#' control the potential computational load. Recommended value = 30
#'
#' @author Ariane J. Gauthier \email{arianejgauthier@outlook.com}
#'
#' @references
#' Dupuis, M., Meier, E., Capel, R., & Gendre, F. (2015). Measuring individuals’ response quality
#' in self-administered psychological tests: An introduction to Gendre’s functional method.
#' \emph{Frontiers in Psychology, 6}, 629. \doi{10.3389/FPSYG.2015.00629}
#' @importFrom psych principal
#' @importFrom stats cor
#' @importFrom stats sd
#' @importFrom dplyr select
#'
#' @export compute_cohRel
#'
#' @examples
#' df <- mierda_data #rename dataframe
#' scales_list <- list(scale1 = dplyr::select (df, Item_1:Item_50))
#' nb_factors <- list(scale1 = 5)
#' rel_coh <- compute_cohRel(df, scales_list, nb_factors)

compute_cohRel <- function(df, scales_list, nb_factors, max_iterations = 30) {

  verbose = F

  if (!is.data.frame(df)) {
    stop("Input data must be a data frame")
  }

  ###### Functions #####

  ## Specify function to normalize row
  normalize_row <- function(row) {
    norm <- sqrt(sum(row^2))  # Euclidean norm
    if (norm != 1) {
      row / norm
    } else {
      row
    }
  }
  ## Specify row normalizing function without if else statement
  normalize_row2 <- function(row) {
    normed <- row/sqrt(sum(row^2))  # Euclidean norm
  }

  ## Set response reliability function
  RespReliability <- function(normed_scores, scale_data) {
    # Compute inner product matrix using matrix of items characteristics
    inner_prod <- normed_scores %*% t(normed_scores)
    inner_prod[lower.tri(inner_prod, diag = TRUE)] <- NA

    # Copy inner_prod
    Mani_inner_prod <- inner_prod

    # Initialize pairs matrix
    pairs <- matrix(0, dim(Mani_inner_prod)[2] / 2, 2)

    # Loop to find pairs with maximum inner product
    for (i in 1:(dim(Mani_inner_prod)[2] / 2)) {
      pairs[i,] <- which(Mani_inner_prod == max(Mani_inner_prod, na.rm=TRUE), arr.ind = TRUE)[1,]
      Mani_inner_prod[, pairs[i,]] <- NA
      Mani_inner_prod[pairs[i,], ] <- NA
    }

    # Extract part_1 and part_2
    part_1_indices <- pairs[, 1]
    part_2_indices <- pairs[, 2]

    # Initialize reliability vector
    reliability <- rep(NA, nrow(scale_data))

    # Determine the number of rows to iterate over
    num_rows <- nrow(scale_data)

    # Loop to compute reliability for each row
    for (i in 1:num_rows) {
      part_1 <- scale_data[i, part_1_indices, drop = FALSE]
      part_2 <- scale_data[i, part_2_indices, drop = FALSE]

      # Check for all NA
      if (all(is.na(part_1)) || all(is.na(part_2))) {
        reliability[i] <- NA
        next  # Skip to next row
      }

      # Compute correlations for part_1 and part_2
      half_1 <- tryCatch({
        normalize_row2(stats::cor(normed_scores[pairs[, 1], ], t(part_1),
                                  use = "pairwise.complete.obs"))
      }, warning = function(w) NA)

      half_2 <- tryCatch({ normalize_row2(stats::cor(normed_scores[pairs[, 2], ], t(part_2),
                                                     use = "pairwise.complete.obs"))
      }, warning = function(w) NA)

      # Get raw reliability and exclude missing
      #valid_idx1 <- !is.na(half_1)
      #valid_idx2 <- !is.na(half_2)
      #reliability[i] <- sum(half_1[valid_idx1] * half_2[valid_idx2])

      reliability[i] <- sum(half_1 * half_2, na.rm = T)
    }

    # Apply Spearman-Brown correction
    rreliability <- ifelse(((2 * (reliability)) / (1 + (abs(reliability)))) < -1, -1, ((2 * (reliability)) / (1 + (abs(reliability)))))

    return(rreliability)
  }

  ##### ANALYSIS #####
  # SET PARAMETERS
  converged <- FALSE # Set convergence criteria
  scale_factors_list <- list() # Create an empty list to store the scales and number of factors
  max_iterations <- max_iterations # set max num of iterations to reach convergence
  tolerance <- .01 # Set tolerance criteria for convergence

  # Create an empty list to store the response coherence values for each scale
  response_coherence_list <- list()
  reliability_list <- list()

  df <- as.matrix(df) # Coerce to dataframe

  # STEP 0.5 - Data manipulation
  scale_factors_list <- list(scales_t = scales_list, nb_factors = nb_factors) # Store scales names and # factors in the list
  scales_t <- scale_factors_list$scales_t #list of scale names
  nb_factors <- scale_factors_list$nb_factors #list of factors
  #nb_factors <- abs(nb_factors) #dummy proofing

  # Loop through each scale -- The response coherence and reliability is computed for each scale.
  for (scale_name in names(scales_t)) {
    scale_data <- scales_t[[scale_name]]
    scale_data <- as.matrix(scale_data)
    num_factors <- nb_factors[[scale_name]]

    # Skip if num_factors is smaller or equal to 1 and return NA for reliability and response coherence
    if (num_factors <= 1) {
      if (verbose) {
        cat("Skipping scale", scale_name, "due to zero factors\n")
      }
      reliability_list[[scale_name]] <- NA
      response_coherence_list[[scale_name]] <- NA
      next
    }

    # STEP 1 - Perform first PCA on original scales
    pca_df <- psych::principal(scale_data, num_factors, n.obs = nrow(df), rotate = "varimax") #Varimax rotation is applied to ensure interpretability
    loadings <- pca_df[["loadings"]] # Extract loading matrix

    # STEP 2 - Perform PCA on normalized loadings
    #normalized_loading_matrix <- t(apply(loadings, 1, normalize_row)) # Apply row normalizing function to the loading matrix
    #pca_df1 <- psych::principal(normalized_loading_matrix, num_factors, n.obs = nrow(df), rotate = "varimax") # Run PCA on the resulting normalized loading matrix
    pca_df1 <- psych::principal(loadings, num_factors, n.obs = nrow(df), rotate = "varimax") # Run PCA on the resulting normalized loading matrix
    scores <- pca_df1[["scores"]] #extract factor scores

    # STEP 3 - Repeat the normalization until convergence
    for (iteration in 1:max_iterations) {
      scores <- pca_df1[["scores"]] # Extract factor scores
      norms <- sqrt(rowSums(scores^2)) # Compute the norms for each row of the factor score matrix

      # Check if each item norm is equal to 1
      if (all(abs(1 - norms) < tolerance)) {
        converged <- TRUE
        # Divide each row of the factor score matrix by its norm
        normed_scores <- t(apply(scores, 1, normalize_row))
        normed_scores

      } else {
        # Perform PCA on the normalized scores for the next iteration
        # Divide each row of the factor score matrix by its norm
        normed_scores <- t(apply(scores, 1, normalize_row))
        pca_df1 <- psych::principal(normed_scores,
                                    num_factors,
                                    normalize = TRUE,
                                    n.obs = nrow(df),
                                    rotate = "varimax")

      }
      # Break the loop if converged
      if (converged) {
        break
      }
    }

    # STEP 4 - GET RESPONSE STRATEGY
    # Transform KX into KZ -- normed_scores == matrix items characteristics
    z_scores <- scale(normed_scores)
    # Calculate the mean and standard deviation for each item
    item_means <- colMeans(scale_data, na.rm = T) # get mean
    item_sd <- apply(scale_data, 2, stats::sd, na.rm = T) # get sd
    # Metricize the response matrix
    metricized_response <- t(t(scale_data - item_means) / item_sd)
    # Calculate the response strategy
    response_strategy <- lapply(1:nrow(metricized_response), function(i) {
      vector <- as.vector(metricized_response[i, ])
      sapply(1:ncol(z_scores), function(j) cor(vector, z_scores[, j], use = "pairwise.complete.obs"))
    })

    # STEP 5 - COMPUTE RESPONSE RELIABILITY FOR THE SCALE
    reliability <- RespReliability(normed_scores, scale_data)
    # Replace all 0 values with NA
    #reliability[reliability == 0] <- NA
    # Assign to the list
    reliability_list[[scale_name]] <- reliability

    # STEP 6 - COMPUTE RESPONSE COHERENCE FOR THE SCALE
    response_coherence <- sapply(response_strategy, function(x) sqrt(sum((x)^2)))
    response_coherence_list[[scale_name]] <- response_coherence
  }

  return(list(
    response_coherence = response_coherence_list,
    rr = reliability_list
  ))
}
