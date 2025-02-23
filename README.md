# mIERda
The mIERda (multidimensional insufficient effort responding detection analysis) is an R package designed to help researchers and analysts identify careless/IER participants in their datasets. The mIERda package automatizes the data screening process for IER by providing functions which facilitates the computation of a selection of IER indicators (IERIs), data screening using an adaptation of a fair cut forest algorithm to distinguish between attentive and IER participants, and by computing a data-driven cutoff to assist researchers and analysts identifying C/IER respondents in their data. The goal of mIERda is to simplify the identification of careless/IER respondents (C/IER) in datasets containing psychometric scales collected through surveys.

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
```{r}
# STEP 1 - COMPUTING EACH INDICATOR
## (A) - LONGSTRING
ls <- compute_longstring(df) #Computes longest string of consecutive responses

## (B) - MAHALANOBIS DISTANCE
md <- compute_md(df, return_pvalues = FALSE) #Computes md and returns vector of distance
md <- compute_md(df, return_pvalues = TRUE) #Computes md and returns list of distance and p-values
distance <- md[[md]] #returns vector of distance
md_p <- md[[p_values]] # returns vector of p-values 

## (C) - RESPONSE COHERENCE AND RELIABILITY
### The compute_cohRel function will only return an output for multidimensional scale (nfactors ≥ 2). If a scale is unidimensional (nfactors = 1) the compute_cohRel function will return NA for this scale
rel_coh <- compute_cohRel(df, scales_list, nb_factors) #Computes response coherence and reliability then returns list of response coherence and response reliability for each scale of the survey
respCoh_scale_name <- rel_coh[[response_coherence]][[scale_name]] #Returns response reliability. If lenght(scale_name)=1 a vector is returned, otherwise a list of length(scale_name) is returned
respRel_scale_name <- rel_coh[[rr]][[scale_name]] #Returns response reliability; if length(scale_name)=1 a vector is returned, otherwise a list of length(scale_name) is returned

## (D) - INTRA-INDIVIDUAL VARIABILITY/INTRA-INDIVIDUAL STANDARD DEVIATION
isd <- compute_isd(df, na.rm = TRUE) #Computes IRV/ISD and returns vector for each respondent

## (E) - PERSON-TOTAL CORRELATION
ptc <- compute_ptc(df) #Computes person-total correlation and returns correlation for each respondent

# STEP 2 - COMPUTING THE MIERDA SCORE
score <- mIERda_score(x) # Returns a vector of mierda score for each participant

# STEP 3 - COMPUTING THE MIERDA CUTOFF
cutoff <- mIERda_cutoff(score) #Finds the cutoff which minimizes the sum of squares based on the distribution of mIERda scores. Returns a vector of length=1. Respondents with values smaller than or equal to the cutoff are deemed attentive whilst responded with values greater than the cutoff should be considered C/IER
```
