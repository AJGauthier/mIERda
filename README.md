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
#### (A) - LONGSTRING
```{r}
ls <- compute_longstring(df) #Computes longest string of consecutive responses
```

#### (B) - MAHALANOBIS DISTANCE
```{r}
IF return_pvalues = FALSE
md <- compute_md(df, return_pvalues = FALSE) #Computes md and returns vector of distance

## IF return_pvalues = TRUE
md <- compute_md(df, return_pvalues = TRUE) #Computes md and returns list of distance and p-values
distance <- md[[md]] #Returns vector of distance
md_p <- md[[p_values]] #Returns vector of p-values 
```

#### (C) - RESPONSE COHERENCE AND RELIABILITY
The compute_cohRel function will only return an output for multidimensional scale (nb_factors ≥ 2). If a scale is unidimensional (nb_factors = 1) the compute_cohRel function will return NA for this scale.
```{r}
rel_coh <- compute_cohRel(df, scales_list, nb_factors) #Computes response coherence and reliability then returns list of response coherence and response reliability for each scale of the survey
respCoh_scale_name <- rel_coh[[response_coherence]][[scale_name]] #Returns response reliability. If length(scale_name)=1 a vector is returned, otherwise a list of length(scale_name) is returned
respRel_scale_name <- rel_coh[[rr]][[scale_name]] #Returns response reliability; if length(scale_name)=1 a vector is returned, otherwise a list of length(scale_name) is returned
```

#### (D) - INTRA-INDIVIDUAL VARIABILITY/INTRA-INDIVIDUAL STANDARD DEVIATION
```{r}
isd <- compute_isd(df, na.rm = TRUE) #Computes intra-individual standard deviation and returns vector for each respondent
```

#### (E) - PERSON-TOTAL CORRELATION
```{r}
ptc <- compute_ptc(df) #Computes person-total correlation and returns correlation for each respondent
```

### STEP 2 - COMPUTING THE MIERDA SCORE
```{r}
score <- mIERda_score(x) #Returns a vector of mierda score for each participant
```

### STEP 3 - COMPUTING THE MIERDA CUTOFF
```{r}
cutoff <- mIERda_cutoff(score) #Finds the cutoff which minimizes the sum of squares based on the distribution of mIERda scores. Returns a vector of length=1. Respondents with values smaller than or equal to the cutoff are deemed attentive whilst responded with values greater than the cutoff should be considered C/IER
```
