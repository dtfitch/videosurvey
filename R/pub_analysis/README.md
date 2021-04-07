# Update of analysis from the report or publication
## Items to work on:

(1) Outlier analysis: try slicing and repeated estimation a la (https://arxiv.org/pdf/1807.06068.pdf)
notes:  Have you looked much at the respondents with worst fit from out best-fitting model? 
I remember looking at outliers from a model without person effects and I think the worst outliers were people 
who tended to rate everything poorly despite claiming to be comfortable biking on a 4-lane road. 
That’s where I would start in deciding how to add complexity to the model. 

INCOMPLETE: slice_analysis.R
results clearly show that people who always select neutral are poorly predicted.
This in turn biases predictions of neutral responses for people who select neutral
for less than all videos. People who don't vary their response but who select a
class other than neutral, don't seem to be problematic. In fact, people who always
select Very comfortable" are predicted quite well. I think this suggests that the
only people we have to worry about are the "always neutral" respondents
(n = 97 people or 485 responses or 3.2% of the data)

TO DO: repeat analysis after other model updates, make final decision on whether
to remove or retain "always neutral" respondents.

other slicing by person-level IVs show no issues

(2) Prior predictive checking
Notes: prior predictive checks with N(0,5) for betas and t(3,0,5) for intercepts and sd showed exteme expectations
Because we are using logit transform, priors end up peaked at 0 and 1 with "valley" across all other parameter space
(see plot(density(inv_logit_scaled(rnorm(1e5,0,5))))).
After using pp_check with a variety of priors, settled on much tighter priors: N(0,0.5) for betas, N(0,1.5) for intercepts,
t(3,0,0.5) for sd, and lkj(2) for vary intercetp and slope correlations (only one varying slope in the model VideoGroup).

Priors are overwhelmed by data anyway, so no real effect on inference, but they speed up estimation slightly and are more 
defendable.
COMPLETE

(3) Include Video Group as a main and varying effect by video ID
Note: I think we should have just done this even though the effect is probably minor. Since the design of
the experiments had this in mind, i think it is more defensible.
COMPLETE: model 1 (base)

(4) Interactions (or varying slopes) for personal characteristics
Notes: one of the arguments of "infra minimums" is that not only is variation by socio-demographics miminal, but that
variation in the effect of socio-demographics on comfort is minimal. We need to look at this by gender (already started that 
at the Women's issue sin Trans conference) and othe rsocio-demographics and perhaps even attitudes

Try one person-level variable interacted with all street-level variables at a time
Predictions were improved when interacting age by road variables, and when interacting comfort_four_no_lane3
by road variables. To reduce the number of interaction terms, I selected the terms from those interactions 
that showed effects >|0.5|. These effects suggest that age or comfort moderate the road variable by enough to
shift the prediction one class on average (Jane, please check my evaluate_interactions.R and see if you agree with my decisions,
also, if the calculated contrast is >|0.5| am I correct to say that on average the moderating effect would shift an answer response?

COMPLETE: Model 2

(5) Relaxing parallel slopes assumption, relaxing common variance assmuption (lower priority)

RESULTS: ran adjacet category models (see Burkner and Vouve), one with no added parameters and one with category
specific effects (allowed the varying intercepts by video_name to vary all thresholds, not just shift them). Both models
performed much worse that the regular cumulative models. I'm happy dropping and leaving this one undiscussed.

COMPLETE: Model 3 acat and acat with category specific thresholds

(6) Monotonic predictions (lower priority)
Notes: Before moving to brms I looked into whether the numeric variables should be treated as factors or numeric. 
I don’t remember exactly what I found but I guess I was satisfied that the incremental effects were regular enough that numeric was usually OK, 
or else binned them (e.g. with the speed limit variable). But with the added person and video effects it’s worth investigating again. 
Still, just in the interest of not falling down a rabbit hole, I’d look for model misfit first. 

RESULT: I decided not to pursue this one. So many moving parts in this study and I am happy with model 2.

