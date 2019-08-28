# videosurvey
Analysis of factors influencing bicycling comfort based on UC Davis transportation survey. 

Individuals were asked to watch 10-second clips of streets and rate their comfort bicycling on those streets. 

### Jane's Notes

# Files in the Repository

## R 

These files are listed in the order they would likely be used

  - data_comfort.R - preliminary script to process data
  - eda_comfort.R secondary script to perform exploratory analysis of the data
  - model_comfort_jc.R initial modelling and variable selection script. Also contains models using existing bike-friendliness metrics. 
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
  - analysis_comfort_jc.R	 - Code for model evaluation and prediction from models.


- Modeling notes 
  - Preliminary ordered model (ord.prelim) and penalized ordered model (glmcr.prelim) have reasonable coefficient estimates.
  - Street features basically provide as much information as street id. (This may be because the features essentially reconstruct the names, though the signs of the coefficients make sense so probably not.) 
  - There is a lot of variation in street ratings in general. 
    - Rating distributions are more bimodal (bumps for high and low) than normal, and certain qualities (e.g. don't like biking) seem to flatten out the distribution rather than just moving the mean
    - What explains the tails in the ratings i.e. people who deviate from the general trend? Do people have low-rating and high-rating tendencies (random effects?), and/or are they explained by covariates?
    - Strategy for responders who rated all videos the same, even when shown "between" blocks. (Consider as outliers? Add random effects? Seperate random effects for between and within blocks?)
  

- Data questions
  - Why so many blank entries in the child fields? Why a few "Nones" in addition to zeros? (free-entry fields?)
  - Exact difference between bike lane, width, shoulder, etc.?
  ~~- Coding of hh_composition? Conflicting levels selected. (Used rent_share as a hh composition type variable because it has least missing data nad is still somewhat discriptive of household type.)
      ~~- This is probably due to students having multiple residences. I recoded best I coudl to a 4-level varibable.
  ~~- Are blocked bike lanes really blocked? (https://youtu.be/LI0m8h3jVJ4, https://youtu.be/XAKiJ78Z8uE)
    ~~- Probably due to being block in a non-excerpted part of the video. Ignore this variable.

  
- Next up
    - How do the existing ratings do in comparison to simple feature-based models. LTS is simpler, how does it compare to NCHRP and HCM? How fine/course should the classification be? Is a binary classification possible?
    - Compare models with individual stuff (person-level model) + 0) Video Name 1) NCHRP 2) HCM 3) LTS 4) Raw Features
    - What does it take for x percent of people to be comfortable?
    - Hypothetical environments
    - What would be available with something like census data? (e.g. without opinion statements)
  
  
