Prepare Data
================

- <a href="#scripts" id="toc-scripts">Scripts</a>
  - <a href="#required-packages--reproducibility"
    id="toc-required-packages--reproducibility">Required Packages &amp;
    Reproducibility</a>
  - <a href="#tidy-data" id="toc-tidy-data">Tidy Data</a>
  - <a href="#save-data-for-analysis" id="toc-save-data-for-analysis">Save
    Data for Analysis</a>
  - <a href="#visualization-of-data"
    id="toc-visualization-of-data">Visualization of Data</a>
    - <a href="#dependent-variable" id="toc-dependent-variable">Dependent
      Variable</a>
    - <a href="#treatment-conditions" id="toc-treatment-conditions">Treatment
      Conditions</a>
    - <a href="#correlations-matrix" id="toc-correlations-matrix">Correlations
      Matrix</a>

# Scripts

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
d <- fetch_survey(surveyID = "SV_bIoc119D4OgUztA", 
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
