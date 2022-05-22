Prepare Experimental Data
================

-   [Scripts](#scripts)
    -   [Required Packages &
        Reproducibility](#required-packages--reproducibility)
    -   [Tidy Data](#tidy-data)
    -   [Save Data for Analysis](#save-data-for-analysis)
    -   [Visualization of Data](#visualization-of-data)
        -   [Dependent Variable](#dependent-variable)
        -   [Treatment Conditions](#treatment-conditions)
        -   [Correlations Matrix](#correlations-matrix)

# Scripts

-   [Required Packages &
    Reproducibility](#required-packages-&-reproducibility)
-   [Tidy Data](#tidy-data)
-   [Save Data for Analysis](#save-data-for-analysis)
-   [Check Missing Values](#check-missing-values)
-   [Visualization of Data](#visualization-of-data)
    -   [Dependent Variable](#dependent-variable)
    -   [Treatment Conditions](#treatment-conditions)
    -   [Correlations Matrix](#correlations-matrix)

## Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
```

## Tidy Data

This code chuck downloads the data from Qualtrics using the API and
cleans the raw data.

``` r
## Match data with RESPONDI quota questions
d <- fetch_survey(surveyID = "SV_7PoXOEVMUF1JZn8", 
                    verbose = TRUE, force_request = T,
                    label = FALSE, convert = FALSE)
source(here("src/data-processing/clean_data.R"))
```

## Save Data for Analysis

``` r
save(d, file = here("data/intermediate/cleaned_experiment.RData"))
```

## Visualization of Data

### Dependent Variable

<img src="../../report/figures/Dependent Variable-1.png" style="display: block; margin: auto;" />

### Treatment Conditions

<img src="../../report/figures/Independent Variables-1.png" style="display: block; margin: auto;" />

### Correlations Matrix

<img src="../../report/figures/Correlations Matrix-1.png" style="display: block; margin: auto;" />
