# Responsive or Responsible?  The Reputational Cost of Political Compromise
Online research compendium of the paper entitled _Responsive or Responsible?  The Reputational Cost of Political Compromise_ . 
This repository combines the re-registration plans, data &amp; analysis compendium.

## Draft
View the [draft of the paper](report/draft.pdf) and the [online appendix](report/OnlineAppendix.pdf) here.

# Pre-Registration Plan
* [Observational Study](https://osf.io/h29j3) Pre-registered hypothesis and analyses for testing the relationship between political trust and willingness to accept compromises in politics (_hypothesis 1_).
* [Experiment](docs/pre-analysis-plan/pap.pdf) Pre-registered hypotheses and  analyses for testing the effect of parties’ negotiation position from  success or failure in the negotiations (_hypotheses 1--3_).; and delve into the mechanisms that drive voters’ party trust after (not) accepting compromises (_hypotheses 4--5_)..

# Data
The following data files might be of interest:

* [Observational Study](data/intermediate/observational_data.RDS) Cleaned data for analysis of the observational study of 6 Western European countries ([Comparative Study of Electoral Systems Module 5](https://cses.org/data-download/cses-module-5-2016-2021/)) for Austria, Germany, France, Italy complemented with election study data from the [2017 British Election Study](https://www.britishelectionstudy.com/news-category/2017-general-election/) and the [2017 Dutch Parliamentary Electoral Studies](https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:101156).).
* [Experiment](data/intermediate/cleaned_experiment.RData) Cleaned data with constructed scales for analysis of experiment fielded in Germany (October 2021).

See the scripts in `src/data-processing` for details on how these files were constructed  for the [observational](src/data-processing/clean_data_obs.md) and the [experimental data](src/data-processing/clean_data_exp.md).

# Results
* [Observational Study](src/analysis/observational/obervational_study.md) Demonstrates the analyses to test and visualise hypothesis 1. In addition, we report that the results are robust against specifications:
	- Political Trust predicted by Willingness to Accept Compromise per country
	- Effect of recoding missing values
* [Experiment](src/analysis/experiment/experiment.md) Demonstrates the analyses to test and visualise hypothesis 1 -- 5. In addition, we report the exploratory results for various political moderators as well as various robustness checks.

# Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.