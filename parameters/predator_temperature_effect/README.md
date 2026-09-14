
- <a href="#predation-submodel" id="toc-predation-submodel">Predation
  Submodel</a>
  - <a href="#generating-predators" id="toc-generating-predators">Generating
    predators</a>
  - <a href="#effects-of-temperature"
    id="toc-effects-of-temperature">Effects of temperature</a>
  - <a href="#gape-limitation" id="toc-gape-limitation">Gape limitation</a>
  - <a href="#effect-of-cover" id="toc-effect-of-cover">Effect of cover</a>

**TODO**: Clean up figure formats so they look good and are consistent

# Predation Submodel

The predation submodel attempts to split the difference between treating
predators as an abstract risk field vs. creating predator agents (and
thus greatly increasing model complexity).

## Generating predators

Predator presence and absence were evaluated using data collected in
2013–2014 by FISHBIO **\[REFS\]**. Briefly, transects starting 15 feet
from shore and moving towards shore were electroshocked and all fish
were collected. Afterwards, environmental data were collected from each
transect, including percent cover from submerged vegetation and woody
debris, presence/absence of shade, type of substrate, water depth (at 5
ft, 10 ft, and 15 ft from shore) and water velocity (at 5 ft, 10 ft, and
15 ft from shore). These data are appealing because they are similar in
nature to the individual-based model setup; i.e., transects/model cells
have their own unique local habitat qualities and are associated with
the presence/absence of predators.

Before building the predator model, the data were cleaned and parsed.
Vegetation and wood data were originally reported as percent ranges. The
mean value of each range was used and converted to a proportion.
Substrate classification varied from year to year, and the vast majority
of values were either rocky or not; therefore, all substrate
classifications that included “rocky” were simplified to “rocky”.
Gravel, which appeared very infrequently, was classified as “rocky”. All
other substrates were classified as “mud”. Finally, water depth and
velocity values were averaged (i.e., the respective values at 5, 10, and
15 ft from shore) to yield a single depth or velocity value per
transect.

The model includes predictions for both smallmouth bass (*Micropterus
dolomieu*) and Sacramento pikeminnow (*Ptychocheilus grandis*). However,
data for smallmouth bass from 2013 included observations for other
*Micropterus* spp. (listed as “black bass”) whereas data in 2014 were
broken down by species. For consistently, all black bass data from 2014
were pooled. Hereafter, references to “smallmouth bass” refer to pooled
“black bass” data. Furthermore, data were divided by fish size (\< 150
mm and \> 150 mm). Due to the limited numbers of observations, these
data were also pooled, although smaller fish likely are not piscivorous
and may exhibit different behaviors and habitat choices than large
conspecifics. Finally, because there were so few observations that
included more than one predator, the original count data were
transformed into presence/absence data.

The data for each predator were fit to generalized linear models with a
logit link, as follows:

$$
logit(P_{predator}) = \beta_{0} x_{intercept} + \beta_{1} x_{veg} + \beta_{2} x_{wood} + \beta_{3} x_{shade} + \beta_{4} x_{substrate} + \beta_{5} x_{depth} + \beta_{6} x_{velocity}
$$

Models were fitted and evaluated using the `tidymodels` package suite in
`R`. Data were split into training and testing sets using a 0.75 split
(i.e., 75% of the data was placed in the training set). Because the data
contained many absences relative to presences, the `step_rose` function
from the `themis` package was used for upsampling the training set. The
model was then fit to the training set and evaluated using the testing
set.

<div class="figure">

<img src="output/figs/pred_log_model/pred_roc_plots.jpg" alt="temperature vs relative predator activity level" width="75%" />
<p class="caption">
temperature vs relative predator activity level
</p>

</div>

**Fig 1**. Receive operator characteric (ROC) curves for smallmouth bass
(left) and Sacramento pikeminnow (right).

Predictions for the bass model were relatively good (ROC area under the
curve \[AUC\] = 0.695), while those for Sacramento pikeminnow were more
limited but better than guessing (ROC AUC = 0.742) (**Fig 1.**). The
pikeminnow results were likely hampered by the limited number of
observed individuals (191 out of 2995 total observations). The results
for bass could have likely been improved with more observations
specifically for smallmouth bass. Pooling large and small individuals as
well as multiple species (i.e., “black bass”) likely reduced the
predictive power of the model.

Because the GLMs do not account for transect size, the predictions were
rescaled relative to the sum of all cell predictions for a given
species:

$$
P_{scaled_i} = P_{predator_i} / \sum_{i = 1}^{i=N} P_{predator_i}
$$

$P_{scaled}$ was then multiplied by the total number of predators in the
system, $N_{pred}$, to yield the expected number of predators for a
given cell. This value was used as a probability in the `rbinom`
function in `R` to assign a predator to a cell. $N_{pred}$ is a reach
value based on literature **\[REF\]** but can be adjusted by the user.
In this way, predictions are scaled by the environment’s number of
predator relative to model’s original prediction for the expected total
number of predators, which also accounts for the effect of transect/cell
size on the outcome. Additionally, depth and velocity can change daily,
so predators are updated with environmental parameters, allowing for
predator “movement”

## Effects of temperature

Predators are assumed to be stationary and can “interact” with prey that
fall within their “reaction distance”, which represents the maximum
distance at which a predator might react to a prey item; i.e., predators
are represented by a point and sweep out a circular area with a radius
equal to their reaction distance. The probability of prey fish
encountering a predator is thus the total area swept out by predators’
reaction distance divided by the area of the cell.

The area of the this circle is modified by temperature. Data from
various metabolic and activity studies of pikeminnow and bass were
scaled and fit with a logistic model **TODO: \[REFS\] (Fig XXXX)**. The
area of predator circles is scaled by the model to represent changing
activity levels with changing temperature.

<div class="figure">

<img src="output/figs/pred_temperature_effects/temp_vs_pred_activity.jpg" alt="temperature vs relative predator activity level" width="75%" />
<p class="caption">
temperature vs relative predator activity level
</p>

</div>

**TODO:** Figure xxxx. Temperature vs relative predator activity level.

## Gape limitation

Although encounters between predator and prey are determined
probabilistically based on the proportion of a cell occupied by
predators, the outcomes of these interactions are governed by the size
of the predator vs. the size of the prey. Predators are gape limited,
and no lethal encounters occur when a prey fish exceeds the gape-limited
maximum prey size of a predator. If, however, a prey fish is at or below
the maximum prey-size threshold, a predation encounter can proceed.
**TODO**: mention sturgeon explicitly. Predator sizes are drawn from a
log-normal distribution fit to the predator length frequencies from the
FISHBIO reports \[REFS\]. Maximum prey sizes are determined following
**\[REF\]**. Because **\[REF\]** doesn’t include Sacramento pikeminnow
in its assessment, we treat all predators as smallmouth bass. Minimum
predator size is set to 150 mm (i.e., fish have switched to piscivory)
and the maximum size is set to the largest observed size from the
FISHBIO reports.

- show histograms with fit?

## Effect of cover

The outcome a predator–prey encounter ($P_{survival}$) is determined by
the base success rate of predators ($P_{pred \\_ success}$), which is a
reach value that can be modified by the user, and the survival benefit
of cover ($S_{cover}$) in the cell, as follows:

$$ 
P_{survival} = (1 - P_{pred \\_ success}) + P_{pred \\_ success}S_{cover} 
$$

The relationship between available cover and survival benefit was
assessed based on the average distance to cover **TODO (Fig. xxxxx)
\[REF\]**

<div class="figure">

<img src="output/figs/cover_benefits/cover_benefits.jpg" alt="temperature vs relative predator activity level" width="75%" />
<p class="caption">
temperature vs relative predator activity level
</p>

</div>

**TODO** Fig. XXXXXX

Although distance to cover was used as a proxy for safety from
predators, representing this figure in the model cells is challenging,
as only the percent cover value is available. Therefore, we simulated in
`R` the distance to cover based on percent cover, as follows. First,
cover was simulated as polygons of random placement, shape and size were
generated in a $1 \times 1$ simulated cell **TODO**: (Fig. xxxx).

<div class="figure">

<img src="output/figs/cover_simulation/polygon_sim.jpeg" alt="temperature vs relative predator activity level" width="75%" />
<p class="caption">
temperature vs relative predator activity level
</p>

</div>

**TODO**: Fig xxxx. Example polygons

Points (n = 10,000) were then plotted randomly in each simulated cell
**TODO (Fig. xxxx)** and their distance to the nearest polygon was
measured. Points that fell within a polygon were considered to have a
distance of 0. The mean value of these measurements was considered the
mean distance to cover for a given cell. In total, 12000 cells were
simulated, varying in number and size of polygons to represent a wide
range of possible cover configurations. Thus, the proportion of each
cell that was occupied by cover could be associated with a distinct mean
distance to cover. A polynomial function was fit to these data **TODO
(Fig. xxxx)**:

$$
D_{cover} = 0.48 +-0.58\sqrt P_{cover} +-0.18P_{cover} +0.28{P_{cover}}^{1.5}
$$

where $P_{cover}$ is the proportion of cover in the cell and $D_{cover}$
is the mean distance to cover.

<div class="figure">

<img src="output/figs/cover_simulation/model_fit.jpeg" alt="temperature vs relative predator activity level" width="75%" />
<p class="caption">
temperature vs relative predator activity level
</p>

</div>

**TODO**: Fig. xxxxx

Because the simulations were conducted in $1 \times 1$ cells,
predictions for distance to cover can simply be scaled for a given cell
size. If a cell has 0% cover, we give no benefit to survival from cover
during a predator–prey interaction, rather than using the model’s
prediction for a cover value of 0. Although this might be unrealistic
because neighboring cells may offer some cover, this was not considered.
