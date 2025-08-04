# mIERda
The mIERda (multidimensional insufficient effort responding detection analysis) is an R package designed to help researchers and analysts identify careless/IER respondents in their datasets. The mIERda package streamlines the data screening process for insufficient effort responding (IER) and provides functions facilitating the computation of a collection of IER indicators (IERIs). The package also includes a function, based on an adaptation of the isolation forest algorithm, to distinguish between attentive and IER participants, and another function facilitating the computation of a data-driven cutoff. The goal of mIERda is to assist researchers and data analyst working with survey data and psychometric scales in the identification of careless/IER respondents (C/IER).

The mIERda relies on the following IERIs:
- Longstring (Johnson, 2005)
- Intra-individual variability/standard deviation (Marjanovic et al., 2015)
- Person-total correlation  (Karabatsos, 2003)
- Mahalanobis Distance (Meade & Craig, 2012)
- Response Coherence (Dupuis et al., 2015, 2018)
- Response Reliability (Dupuis et al., 2015, 2018)

## Installation
The package is not yet available on CRAN (submission pending). To download the developper version, use:

```{r}
library(devtools)
devtools::install_github("AJGauthier/mIERda")
```

## Description
The mIERda is a package developed by researchers, for researchers. Although the impact of C/IER is well known and established in the literature, screening data for C/IER might be a daunting fact for individuals outside the field of IER. As such, the mIERda was developed as a tool to facilitate the decision process inherent to IER screening, to simplify the interpretation of IER indicators, relieves researchers from the need to establish cut-offs for each IERI, and the subsequent classification process.

## Functions
### STEP 1 - COMPUTING EACH INDICATOR
The first step of the screening process consists of computing the various IER indicators (IERI). 

To ensure that the IER indicators (IERIs) are correctly computed, please verify the following before using the mIERda:

-> Item order matters: Make sure that the item responses appear in the same order they were presented to the participant. This is especially crucial for order-dependent indices such as the Longstring index.

-> Responses must be numeric: The IERIs require numeric input. If your responses were collected using a Likert scale (e.g., “Strongly disagree” to “Strongly agree”), convert them into numeric values (e.g., 1 to 5 or 1 to 7) before computing the IERIs.

Important: Wait before reverse-scoring negatively worded items.
Some IERIs—such as the Longstring index—must be computed using the raw responses. Other indicators, however, require that negatively worded items be reverse-coded first.

To ensure accuracy:

  - Compute the LongString index using the original/raw responses.

  - Then reverse-score negatively worded items.

  - Proceed to compute the remaining IERIs (i.e., Mahalanobis distance, response coherence and reliability, intra-individual variability, person-total     correlation) using the reverse-coded data.

    For a more detailed overview, see Curran (2016): http://dx.doi.org/10.1016/j.jesp.2015.07.006

#### (A) - LONGSTRING INDEX
The longstring index (LS) measures the longest sequence of identical responses provided by a participant across a set of items.
```{r}
ls <- compute_longstring(df) #Returns a vector of length j (where j is the number of respondents in the dataset)
```

#### (B) - MAHALANOBIS DISTANCE
The Mahalanobis Distance (MD) identifies multivariate outliers and is used as an IERI to flag unusual or inconsistent responding in survey or questionnaire data.
```{r}
IF return_pvalues = FALSE
md <- compute_md(df, return_pvalues = FALSE) #Computes md and returns vector of distance of length j

IF return_pvalues = TRUE
md <- compute_md(df, return_pvalues = TRUE) #Computes md and returns list of distance and p-values of length j
distance <- md[["md"]] #Returns vector of distance of length j
md_p <- md[["p_values"]] #Returns vector of p-values of length j; the p-value can be computed but should be excluded from the dataset used to compute the mIERda score.
```

#### (C) - RESPONSE COHERENCE AND RELIABILITY
Response coherence returns, for each participant, the proportion of variance in their responses that is explained by the factorial structure of a given scale. It assesses how well an individual's response pattern aligns with the expected underlying constructs.

Response reliability returns, for each participant, the internal consistency of their responses. It captures how consistently they responded to items that are theoretically or empirically related.

Note: The compute_cohRel function will only return an output for multidimensional scale (nb_factors ≥ 2). If a scale is unidimensional (nb_factors = 1) the compute_cohRel function will return NA for this scale.
```{r}
rel_coh <- compute_cohRel(df, scales_list, nb_factors) #Returns a list of vectors containing the response coherence and response reliability for each scale of the survey

# You can then extract the response coherence and reliability vector for each scale from the resulting list:
respCoh_scale_name <- rel_coh[["response_coherence"]][["scale_name"]] #Returns response reliability for the specified scale 
respRel_scale_name <- rel_coh[["rr"]][["scale_name"]] #Returns response reliability for the specified scale
```

#### (D) - INTRA-INDIVIDUAL VARIABILITY/INTRA-INDIVIDUAL STANDARD DEVIATION
The intra-individual variability (IRV) or intra-individual standard deviation (ISD) returns the within-person standard deviation of item responses. It reflects how much a participant’s responses vary across items, irrespective of item content.
```{r}
isd <- compute_isd(df, na.rm = TRUE) #Returns vector of length j
```

#### (E) - PERSON-TOTAL CORRELATION
The Person-total correlation (PTC) measures the correlation between an individual’s item responses and the total scores (mean) of those items.
```{r}
ptc <- compute_ptc(df) #Returns vector of length j
```

### STEP 2 - COMPUTING THE MIERDA SCORE
After computing each IERI and combining them into a single dataframe, you can apply the mIERda_score function to calculate a mIERda score for each participant.
```{r}
score <- mIERda_score(x) #Returns vector of length j
```

### STEP 3 - COMPUTING THE MIERDA CUTOFF 
After computing the mIERda score, you can use the mIERda_cutoff function to identify the cutoff that minimizes the sum of squares based on the distribution of mIERda scores. Respondents with a mIERda score less than or equal to this cutoff are considered attentive, while those with scores greater than the cutoff should be flagged as exhibiting insufficient effort responding (IER).
```{r}
cutoff <- mIERda_cutoff(score) #Returns a vector of length=1. 
```
