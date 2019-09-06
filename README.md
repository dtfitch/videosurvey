# videosurvey
Analysis of factors influencing bicycling comfort based on UC Davis transportation survey. 

Individuals were asked to watch 10-second clips of streets and rate their comfort bicycling on those streets. 

# Jane's Notes

## Files in the Repository

### R 

These files are listed in the order they would likely be used

  - data_comfort.R - preliminary script to process data
  - eda_comfort.R secondary script to perform exploratory analysis of the data
  - model_comfort_jc.R initial modelling and variable selection script. Also contains models using existing bike-friendliness metrics. (This script is executed before brms models.)
  - model_comfort.R	 - NOT IN USE original modeling script
  - Scripts to run individual random effects models. The names identify the model. "null"" indicates no main effects; "me" indictates main effects but no interactions between main effects; "int" indicates main and interaction effects; per" indicates person-level random effects in the model and "vid" indicates video random effects in teh model.  Models with "horse", using horseshoe priors on some coefficients, were dropped from analysis:
    - null.R 
    - null_reduced.R
    - int_per.R 
    - int_per_horse.R 
    - int_per_vid.R 
    - int_per_vid_horse.R 
    - me_per.R 
    - me_per_horse.R 
    - me_per_vid.R 
    - me_per_vid_horse.R 
  - model_loo.R - Code to add loo (leave-one-out cross validation metric) to fit models
  - analysis_comfort_jc.R	 - Code for model evaluation and prediction from brms models, including most of the key plots for the report.
    - mcmc_areas_ridges2. This is called by analysis_comfort_jc.R and is an extrension of bayesplot::mcmc_areas_ridges that allows me to recode names with the varname_dict I supply (variable_name_dictionary.RDS) and to change the thickness of the density outline.


### data

  - counterfactual_building_blocks.RDS - hypothetical street/demographic/attitude scenarios
  - variable_name_dictionary.RDS - mapping (as a named vector) of model parameter names to readable names 
  
#### Modeling notes

  - Notes from analysis in eda_comfort.R:
    - There is a lot of variation in street ratings in general. 
    - Rating distributions are more bimodal (bumps for high and low) than normal, and certain qualities (e.g. don't like biking) seem to flatten out the distribution rather than just moving the mean
  - Notes from model_comfrort_jc.R
    - Preliminary ordered model (ord.prelim) and penalized ordered model (glmcr.prelim) have reasonable coefficient estimates.
    - Street features basically provide as much information as street id. (This may be because the features essentially reconstruct the names, though the signs of the coefficients make sense so probably not.) 
    - Some responders rated all videos the same, even when shown "between" blocks. (Strategy for them? Consider as outliers? Add random effects? I checked that we don't seem to need seperate distributions of random effects for between and within blocks.)

  
