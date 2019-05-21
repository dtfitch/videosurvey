# videosurvey
Analysis of bicycling comfort from a video survey

### Jane's Notes

- Preliminary linear model (lm.prelim) seems reasonable now, though I think there's still some conflation with divided road and other street features to explain it's large positive coef.
- Street features basically provide as much information as street id. (This may be because the features essentially reconstruct the names, though the signs of the coefficients make sense so probably not.) But we find that there is a lot of variation in street ratings in general. 

- Data questions
  - 1. Why so many blank entries in the child fields? Why a few "Nones" in addition to zeros? (free-entry fields?)
  - 2. Coding of hh_composition? Conflicting levels selected. (Used rent_share as a hh composition type variable because it has least missing data nad is still somewhat discriptive of household type.)
  - 3. Are blocked bike lanes really blocked? (https://youtu.be/LI0m8h3jVJ4, https://youtu.be/XAKiJ78Z8uE)
  - 4. What do do about responders who rated all videos the same, even when shown "between" blocks. (Consider as outliers or pursue more advanced modeling techniques?)
  
  
