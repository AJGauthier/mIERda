# mIERda
The mIERda (multidimensional insufficient effort responding detection analysis) is an R package designed to help researchers and analysts identify careless/IER participants in their datasets. The package automatizes the data screening process, computes diverse IER indicators (IERIs), and uses an algorithm to distinguish between attentive and IER participants. The goal of mIERda is to facilitate the identification of careless/IER respondents (C/IER) in datasets containing psychometric scales collected through survey. The mIERda computes the relevant IER indicators and screens the participants using an adaptation of a fair cut forest algorithm. A data-driven cutoff is computed to assist researchers and analysts in the classification of C/IER respondents.

## Installation
The package is not yet available on CRAN (submission pending). To download the developper version, use:

```{r}
library(devtools)
devtools::install_github("arianejgauthier/mierda")
```

## Description
The mIERda is a package developed by researchers, for researchers. Although the impact of C/IER is well known and established in the literature, screening data for C/IER might be a daunting fact for individuals outside the field of IER. As such, the mIERda was developed as a tool to facilitate the decision process inherent to IER screening, to simplify the interpretation of IER indicators, relieves researchers from the need to establish cut-offs for each IERI, and the subsequent classification process.

## Functions
```{r}
## LONGSTRING
ls <- compute_longstring(df) #Compute longest string of consecutive responses

## MAHALANOBIS DISTANCE
md <- compute_md(df, return_pvalues = FALSE) #Compute md and returns vector of distance
md <- compute_md(df, return_pvalues = TRUE) #Compute md and returns list of distance and p-values
distance <- md[[md]] #returns vector of distance
md_p <- md[[p_values]] # returns vector of p-values 

## RESPONSE COHERENCE AND RELIABILITY
rel_coh <- compute_cohRel(df, scales_list, nb_factors) #Compute response coherence and reliability then returns list of coherence and reliability for each scale of the survey
respCoh_scale_name <- rel_coh[[response_coherence]][[scale_name]] #Returns response reliability. If lenght(scale_name)=1 a vector is returned, otherwise a list of length(scale_name) is returned
respRel_scale_name <- rel_coh[[rr]][[scale_name]] #Returns response reliability; if lenght(scale_name)=1 a vector is returned, otherwise a list of length(scale_name) is returned

## INTRA-INDIVIDUAL VARIABILITY/INTRA-INDIVIDUAL STANDARD DEVIATION
isd <- compute_isd(df, na.rm = TRUE) #Compute IRV/ISD and returns vector for each respondent

## PERSON-TOTAL CORRELATION
pcor <- compute_ptc(df) #Compute person-total correlation and returns correlation for each respondent

## mIERda SCORE
score <- mierda_score(df_ieri) #Generates a faircut forest using the IERIs and returns a vector anomaly score for each participant

## mIERda SCORE CUTOFF
cutoff <- mIERda_cutoff(score) #Finds the cutoff based on the distribution of mIERda scores. Returns a vector of lenght=1. Respondents with values smaller than or equal to the cutoff are deemed attentive whilst responded with values greater than the cutoff should be considered C/IER

# Or you can use the remove_mIERda function to automatize the screening process
df_screened <- remove_mIERda(df, scales_list, nb_factors)
```
