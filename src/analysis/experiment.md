Analyses Experiment
================

# Scripts

# Required Packages & Reproducibility

``` r
rm(list=ls())
source(here::here("src/lib/functions.R"))
```

# Pre-Registered Analyses

``` r
load(here("data/intermediate/cleaned_experiment.RData"))

source(here("src/analysis/scaling.R"))
kbl(scale, booktabs =T, caption = "\\label{tab:scale}Reliable Scales") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "5cm") %>%
  column_spec(2, width = "4cm")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Reliable Scales
</caption>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Variable
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
Cronbach’s alpha
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 5cm; ">
Idealism
</td>
<td style="text-align:right;width: 4cm; ">
0.91
</td>
</tr>
<tr>
<td style="text-align:left;width: 5cm; ">
Relativism
</td>
<td style="text-align:right;width: 4cm; ">
0.79
</td>
</tr>
<tr>
<td style="text-align:left;width: 5cm; ">
Mutual Trust
</td>
<td style="text-align:right;width: 4cm; ">
0.82
</td>
</tr>
<tr>
<td style="text-align:left;width: 5cm; ">
Populist Attitudes
</td>
<td style="text-align:right;width: 4cm; ">
0.81
</td>
</tr>
</tbody>
</table>

``` r
rm(scale)

source(here("src/analysis/data-for-analyses.R"))
```

Next, we automatically extract a `.md` file for the online appendix, as
well as a latex table for the manuscript. We are using jinja2 template
[src/analysis/table_descriptives.tex.j2](table.tex.j2) which is called
with a json string containing the data. To replicate, make sure
`env/bin/pip install -U j2cli` is installed via your command line.

``` r
source(here("src/analysis/descriptive-information-overview.R"))
table2 <- knitr::kable(descr, digits=2)
fn <- here("report/figures/table_descriptives.tex")
cat("# Table: Descriptive Information of Variables under Study \n\n", file=fn)
cat(table2, file=fn, sep="\n", append=T)

methodnames <- setNames(as.list(descr$Variables), descr$Variables)
table <- purrr::map(descr, .f= ".") 
#render_j2(here("src/analysis/table_descriptives.tex.j2"), here("report/figures/table_descriptives.tex"),
#          data=list(data=table, methods=methodnames))
rm(descr, methodnames, table, fn, table2)
```

## Descriptive Results

<img src="../../report/figures/descriptive-results-1.png" style="display: block; margin: auto;" />

## Balance Checks

The figure below shows that the data is unbalanced for the variables:
`Education`,`Income`, `Employment`, `Urbaness`, `Living Place`,
`Birth Place`, `Age`, `Political Knowledge`, `Political Interest`, and
`Ideology`. As described in the Pre-Analysis Plan (p.10), I will add
these covariates to the analyses as controls.

``` r
source(here("src/analysis/balance-test.R"))
df
```

<img src="../../report/figures/balance-checks-1.png" style="display: block; margin: auto;" />

## Steadfast Hypothesis & Outcome Hypothesis

``` r
source(here("src/analysis/h1_exp.R"))
p1
```

<img src="../../report/figures/h1-1.png" style="display: block; margin: auto;" />

## Compromise Hypothesis

``` r
source(here("src/analysis/h3_exp.R")) #pooled?
p2
```

<img src="../../report/figures/h3-1.png" style="display: block; margin: auto;" />

``` r
kbl(t3, booktabs =T, caption = "\\label{tab:interaction_h3}Regression Results table") %>%
  kable_styling(latex_options = c("striped", "hold_position"),
                full_width = F, fixed_thead = T, position = "center") %>%
  column_spec(1, width = "3cm") %>%
  column_spec(2, width = "3cm") %>%
  column_spec(3, width = "3cm") %>%
  column_spec(4, width = "1cm") %>%
  column_spec(5, width = "1cm") %>%
  column_spec(6, width = "1cm")
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Regression Results table
</caption>
<thead>
<tr>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Dependent Variable
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Experimental Condition
</th>
<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;">
Issue
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
AME
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
CI Lower
</th>
<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;">
CI Upper
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Trust
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
TopTax
</td>
<td style="text-align:right;width: 1cm; ">
-0.3388678
</td>
<td style="text-align:right;width: 1cm; ">
-0.4943571
</td>
<td style="text-align:right;width: 1cm; ">
-0.1833784
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Trust
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
TopTax
</td>
<td style="text-align:right;width: 1cm; ">
-0.2382118
</td>
<td style="text-align:right;width: 1cm; ">
-0.3909429
</td>
<td style="text-align:right;width: 1cm; ">
-0.0854806
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Credibility
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
TopTax
</td>
<td style="text-align:right;width: 1cm; ">
-0.5520086
</td>
<td style="text-align:right;width: 1cm; ">
-0.7126171
</td>
<td style="text-align:right;width: 1cm; ">
-0.3914001
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Credibility
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
TopTax
</td>
<td style="text-align:right;width: 1cm; ">
-0.3542757
</td>
<td style="text-align:right;width: 1cm; ">
-0.5120346
</td>
<td style="text-align:right;width: 1cm; ">
-0.1965168
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Representation
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
TopTax
</td>
<td style="text-align:right;width: 1cm; ">
-0.9924423
</td>
<td style="text-align:right;width: 1cm; ">
-1.1620298
</td>
<td style="text-align:right;width: 1cm; ">
-0.8228548
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Representation
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
TopTax
</td>
<td style="text-align:right;width: 1cm; ">
-1.0104033
</td>
<td style="text-align:right;width: 1cm; ">
-1.1769829
</td>
<td style="text-align:right;width: 1cm; ">
-0.8438238
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Trust
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
SpeedLimit
</td>
<td style="text-align:right;width: 1cm; ">
-0.2860698
</td>
<td style="text-align:right;width: 1cm; ">
-0.4430269
</td>
<td style="text-align:right;width: 1cm; ">
-0.1291128
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Trust
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
SpeedLimit
</td>
<td style="text-align:right;width: 1cm; ">
-0.2330025
</td>
<td style="text-align:right;width: 1cm; ">
-0.3900209
</td>
<td style="text-align:right;width: 1cm; ">
-0.0759842
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Credibility
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
SpeedLimit
</td>
<td style="text-align:right;width: 1cm; ">
-0.2467000
</td>
<td style="text-align:right;width: 1cm; ">
-0.4065137
</td>
<td style="text-align:right;width: 1cm; ">
-0.0868862
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Credibility
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
SpeedLimit
</td>
<td style="text-align:right;width: 1cm; ">
-0.3848161
</td>
<td style="text-align:right;width: 1cm; ">
-0.5446905
</td>
<td style="text-align:right;width: 1cm; ">
-0.2249418
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Representation
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
SpeedLimit
</td>
<td style="text-align:right;width: 1cm; ">
-0.4988713
</td>
<td style="text-align:right;width: 1cm; ">
-0.6670099
</td>
<td style="text-align:right;width: 1cm; ">
-0.3307328
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Representation
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
SpeedLimit
</td>
<td style="text-align:right;width: 1cm; ">
-0.7816390
</td>
<td style="text-align:right;width: 1cm; ">
-0.9498417
</td>
<td style="text-align:right;width: 1cm; ">
-0.6134363
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Trust
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
Pooled Analysis
</td>
<td style="text-align:right;width: 1cm; ">
-0.3129812
</td>
<td style="text-align:right;width: 1cm; ">
-0.4226142
</td>
<td style="text-align:right;width: 1cm; ">
-0.2033481
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Trust
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
Pooled Analysis
</td>
<td style="text-align:right;width: 1cm; ">
-0.1938144
</td>
<td style="text-align:right;width: 1cm; ">
-0.3030733
</td>
<td style="text-align:right;width: 1cm; ">
-0.0845556
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Credibility
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
Pooled Analysis
</td>
<td style="text-align:right;width: 1cm; ">
-0.3827609
</td>
<td style="text-align:right;width: 1cm; ">
-0.4952767
</td>
<td style="text-align:right;width: 1cm; ">
-0.2702452
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Credibility
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
Pooled Analysis
</td>
<td style="text-align:right;width: 1cm; ">
-0.3459810
</td>
<td style="text-align:right;width: 1cm; ">
-0.4581131
</td>
<td style="text-align:right;width: 1cm; ">
-0.2338488
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Representation
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Stalled
</td>
<td style="text-align:left;width: 3cm; ">
Pooled Analysis
</td>
<td style="text-align:right;width: 1cm; ">
-0.7365253
</td>
<td style="text-align:right;width: 1cm; ">
-0.8552463
</td>
<td style="text-align:right;width: 1cm; ">
-0.6178043
</td>
</tr>
<tr>
<td style="text-align:left;width: 3cm; ">
DV: Representation
</td>
<td style="text-align:left;width: 3cm; ">
Coalition Talks Continue
</td>
<td style="text-align:left;width: 3cm; ">
Pooled Analysis
</td>
<td style="text-align:right;width: 1cm; ">
-0.9264513
</td>
<td style="text-align:right;width: 1cm; ">
-1.0447681
</td>
<td style="text-align:right;width: 1cm; ">
-0.8081345
</td>
</tr>
</tbody>
</table>

``` r
rm(t3)
```

## Principled Hypothesis

``` r
source(here("src/analysis/h4_exp.R")) #pooled?
p3b / p3a + plot_layout(guides = 'collect')  & theme(legend.position = 'bottom')
```

<img src="../../report/figures/h4-1.png" style="display: block; margin: auto;" />

## Mutual Trust Hypothesis

``` r
source(here("src/analysis/h5_exp.R")) #pooled?
p4
```

<img src="../../report/figures/h5-1.png" style="display: block; margin: auto;" />

# Exploration

``` r
source(here("src/analysis/exploration_h3.R"))
p2_explor
```

<img src="../../report/figures/h2-3-explor-1.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_h45.R")) 
p45d1 + p45d2 + p45d3 +
  plot_layout(guides = 'collect')  & theme(legend.position = 'bottom')
```

<img src="../../report/figures/h45-direct-1.png" style="display: block; margin: auto;" />

``` r
p34e
```

<img src="../../report/figures/h45-direct-2.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_compromise_attitude.R")) 
p_ca
```

<img src="../../report/figures/effect-on-compromise-attitude-1.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_h1_party.R")) 
p1_e
```

<img src="../../report/figures/effects-per-party-1.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_h3_party.R")) 
p2_e
```

<img src="../../report/figures/effects-per-party-2.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_h4_party.R"))
p4a_e1 + p4a_e2 + p4a_e3 +
  plot_annotation(title = 'Principledness (2)')
```

<img src="../../report/figures/effects-per-party-3.png" style="display: block; margin: auto;" />

``` r
p4b_e1 + p4b_e2 + p4b_e3 +
  plot_annotation(title = 'Principledness (1)')
```

<img src="../../report/figures/effects-per-party-4.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_h5_party.R"))
p5_e1 + p5_e2 + p5_e3 +
  plot_annotation(title = 'Mutual Trust')
```

<img src="../../report/figures/effects-per-party-5.png" style="display: block; margin: auto;" />

``` r
source(here("src/analysis/exploration_pol_mod.R"))
mod_a
```

<img src="../../report/figures/explor-pol-mod-1.png" style="display: block; margin: auto;" />

``` r
mod_b
```

<img src="../../report/figures/explor-pol-mod-2.png" style="display: block; margin: auto;" />
