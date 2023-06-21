Results Observational Study
================

# Analysis

- [Set Up](#set-up)
- [Analyses](#analysis)
  - [Pre-Registered](#pre-registered)
  - [Exploratory](#exploratory)

## Set-up

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
load(here("data/intermediate/observational_data.Rdata"))
```

## Analyses

### Pre-Registered

The code below demonstrate the OLS regression predicting the level of
political trust by willingness to accept compromises. Next to the
control variables, it includes country dummies and dummies indicating
whether more than ten percent of the values where missing.

``` r
source(here("src/analysis/h1_obs.R"))
h1
```

<img src="../../report/figures/h1-obs-1.png" style="display: block; margin: auto;" />

The beta-coefficient of Willingness to Accept Compromise is positive and
statistically significant, as hypothesized in [our Pre-Analysis
Plan](https://osf.io/h29j3). The coefficient of 0.15 is however a small
effect. It indicates that the higher one’s willingness to accept
compromise, the higher one’s trust in politicians. More precisely, it
means that when Willingness to Accept Compromise goes up by 1 –
i.e. answering `rather yes` instead of `completely agree` with the
statement *What people call compromise in politics is really just
selling out on one’s principles.* – the the level of trust in
politicians goes up by 0.15. Trust in politicians is a five-point scale,
where 1 indicates completely disagreeing with the statement *Most
politicians are trustworthy* and 5 indicates that completely agrees with
the statement.

Because we pooled the data of various countries, the graph below
demonstrate the country differences. The Netherlads is the reference
category. This graph demonstrates that all countries, but particularly
France, Germany, Great Britain, and Italy, are statistically different
from the Netherlands.

<img src="../../report/figures/robust1-1.png" style="display: block; margin: auto;" />

To make sure the effect is not driven by any particular country, the
graphs below show that for all countries, the coefficient is positive.
Moreover, except for Italy, the coefficient is statistically significant
too. This indicates that the effects found in the pooled analysis are
not driven by any specific country.
<img src="../../report/figures/robust2-1.png" style="display: block; margin: auto;" />

Moreover, we have imputed the missing values using the *Green & Gerber
formula* (Green & Gerber 2008). To demonstrate whether the dichotomous
variables indicating whether the respondents’ value was missing are
different from the respondents’ who have completed the question, the
graph below demonstrates the coefficients of the `missing dummies`. The
graph shows that only for the variables `age` and `trust` the imputed
values elicit a different effect from the non-imputed ones.

<img src="../../report/figures/robust4-1.png" style="display: block; margin: auto;" />

The result show that particularly the imputed missing values for `trust`
and `age` seem to elicit an effect in the regression results. To check
the robustness of the analysis, we therefore have also ran the analysis
excluding the observations where `trust` and `age` have been imputed. As
the visualization below demonstrates, this does not change the
coefficient of `willingness to accept compromise`.

<img src="../../report/figures/robust5-1.png" style="display: block; margin: auto;" />

### Replication Full CSES Round 5

<img src="../../report/figures/cses5-1.png" style="display: block; margin: auto;" /><img src="../../report/figures/cses5-2.png" style="display: block; margin: auto;" />
