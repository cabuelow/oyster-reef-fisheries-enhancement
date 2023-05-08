## Modelling fisheries enhancement from restored oyster reefs

This repository provides code to estimate fisheries enhancement from restored oyster reefs.

### TODO

-   [x] Check database is good, adapt code so can run efficiently for multiple species, confirm with Jas any changes needed

-   t_0 and t_harv missing

-   tO- potentially can estimate using von Bert growth equation

-   t_harv can potentially estimate from size at recruitment, again using berlantony growth equation

-   [ ] Add standard errors or a measure of confidence around each cumulative enhancement value
  - zu Ermgassen 2016 constructs uncertainty intervals using the mean and std. error of juvenile densities. assumes all other LH paramaters are invariant (b/c don't have data on how they vary..perhaps we can do better). But can do this for all of the parameter estimates

-   [ ] Solution for how to represent this error/confidence once we add up the total enhancement value across all species
  - can just sum variances across species once have estimated using simulation for each one (step above)
  
  - TODO: read Thorson et al. 2020, and zu ermgassen 2021
  zu ermgassesn (2021) factors in the std. error of reported densities as opposed to giving them all equal weight, and by giving each bay or estuary equal weighting when deriving the Gulf of Mexico wide values
  
  - [ ] von Bert growth eqn may not be appropriate for all species, in which case switch between that and a different one, e.g., Schnute

### Notes

-   [ ] Some of the lifehistory values have been hard to validate (each resource we find gives different values for the same population) or obtain in consistent formats (some give ranges, some are total length (nose to tail tip) while others are carapace length, or disk length (for rays).

-   [ ] this is a work in progress and we have some contingency (eg. getting age/length at settlement (called recruitment), one year, maturity, and harvest, as they are all often referred to as 'recruitment' depending on the nature of the resource) in place for later discussions on choosing the best metric

-   [x] What is 'theoretical age when length is 0'? - This is a scaling parameter to standardise growth curves of different fish species and directly compare them. Can estimate using the von Bertalanffy growth model if you have length at various points in time, growth rate, and the asymptotic (i.e., maximum) length of a species.

`L(t) = L_inf * (1 - exp(-K*(t - t_0)))`

L_inf = asymptotic length; K = growth rate; t_0 = theoretical age at length 0
