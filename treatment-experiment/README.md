# 'Das Volk' or 'Bürger'? 
Online research compendium of the paper entitled _'Das Volk' or 'Bürger'? The Implications of Ethnic and Civic Conceptions of the People for the Measurement of Populist Attitudes_ . This repository combines the [pre-registration plan](https://github.com/MarikenvdVelden/wording-experiment-populist-attitudes/blob/master/docs/pap/pap.pdf) (see also [here](https://osf.io/d6q5b/)) with a data &amp; analysis compendium.

## Draft
View the paper accepted at the EJPR [here](report/accepted-manuscript.pdf). This Compendium functions as an online appendix, but [here](report/online_appendix.pdf) is also a downloadable version.

## Pre-Registration Plan
* [Experiment](docs/pap/pap.pdf) The theoretical underpinnings and research design to test the implications of an ethnic or civic conception on the people for the measurement of populist attitudes.

## Data
The following data files might be of interest:

* [Experiment ](data/intermediate/cleaned_experiment.RData) Cleaned data with constructed scales for analysis of the experiment 1 in which we test whether the ethnic conception of the people makes people less populist compared to a civic conception, conditional upon strength of national identity and strength of partisan id for a far right party.

See the scripts in [src/data-processing](src/data-processing/prep_data.md) for details on how these files were constructed.

## Results
* [Experiment](src/analysis/analyses.md) Demonstrates the analyses to test and visualise whether the ethnic conception of the people makes people less populist compared to a civic conception, conditional upon strength of national identity and strength of partisan id for a far right party. 
In addition, we report the exploratory results for:
	- Alternative model specification H1, [code](src/analysis/explorative_analysis.R)
	- Interactions with Age, Region, Ideology, and Attitudes towards Pluralism, [code](src/analysis/explorative_analysis.R)

* To delve further into the testing measurement invariance, we report evidence for non-equivalence:
	- CFA, [code](src/analysis/explorative_cfa.R)
	- IRT Analysis, [code](src/analysis/IRT.R)
	- Additive Scaling, [code](src/analysis/additive-scale.R)
	- Wüttke et al. 2020 Approach for scaling populist attitudes, [code](src/analysis/wuttke-approach.R)
	- Explanatory power of single items to predict populist vote, [code](src/analysis/predict-pop-vote-items.R)
	- Explanatory power of scales with various scaling methods to predict populist vote, [code](src/analysis/predict-pop-vote-scales.R)

## Code of Conduct
Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.