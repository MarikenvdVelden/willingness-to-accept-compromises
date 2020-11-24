Preprare Data
================

# Analysis

  - [Set Up](#set-up)
  - [Analyses](#analysis)
      - [Pre-Registered](#pre-registered)
      - [Exploratory](#exploratory)

## Set-up

``` r
rm(list=ls())
source("../lib/functions.R")
d <- readRDS("../../data/intermediate/observational_data.RDS")
```

## Analyses

``` r
analysis <- d %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
         pid, gender, age, education, income, country,
         missing_trust, missing_wtac, missing_rile_selfplacement,
         missing_pid, missing_gender, missing_age, missing_education,
         missing_income) 
m <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                  rile_selfplacement + factor(pid) + factor(gender) + age +
                  education + income + factor(country) + 
                  factor(missing_trust) + factor(missing_wtac) +
                  factor(missing_rile_selfplacement) + factor(missing_pid) +
                  factor(missing_gender) + factor(missing_age) + 
                  factor(missing_education) + factor(missing_income),
                data = analysis)

ggcoefstats(
  x = tidy(m)[2:9,],
  point.args = list(color = "seagreen", size = 3, shape = 15),
  vline.args = list(size = 1, color = "violet", linetype = "dotdash"),
  stats.label.color = "seagreen",
  statistic = "t",
  title = "Political Trust Predicted by Willingness to Compromise",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = FALSE) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("Willingness to Accept Compromise", 
                              "Not Politically Interested", 
                              "Left-Right Self-Placement",
                              "Partisan ID",
                              "Male",
                              "Age",
                              "Education", "Income")) +
  labs(x = "Regression Coefficients", y = NULL) 
```

![](obervational_study_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
ggcoefstats(
  x = tidy(m)[10:14,],
  point.args = list(color = "seagreen", size = 3, shape = 15),
  vline.args = list(size = 1, color = "violet", linetype = "dotdash"),
  stats.label.color = "seagreen",
  statistic = "z",
  title = "Political Trust Predicted by Willingness to Compromise",
  subtitle = "Country Differences",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = FALSE) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("France", 
                              "Germany", 
                              "Great Britain",
                              "Italy",
                              "The Netherlands")) +
  labs(x = "Regression Coefficients", y = NULL)
```

![](obervational_study_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
ggcoefstats(
  x = tidy(m)[15:22,],
  point.args = list(color = "seagreen", size = 3, shape = 15),
  vline.args = list(size = 1, color = "violet", linetype = "dotdash"),
  stats.label.color = "seagreen",
  statistic = "z",
  title = "Political Trust Predicted by Willingness to Compromise",
  subtitle = "Missing Values",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = FALSE) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("Trust", 
                              "Willingness to Accept Compromise", 
                              "Left-Right Self-Placement",
                              "Party ID", "Gender", "Age",
                              "Education", "Income")) +
  labs(x = "Regression Coefficients", y = NULL)
```

![](obervational_study_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->

### Pre-Registered

### Exploratory