## Modelling fisheries enhancement from restored oyster reefs

This repository provides code to estimate fisheries enhancement from restored oyster reefs.

### TODO

-   [ ] Check database is good, adapt code so can run efficiently for multiple species, confirm with Jas any changes needed
  - t_0 and t_harv missing
  - tO- potentially can estimate using berlantony growth equation
  - t_harv can potentially estimate from size at recruitment, again using berlantony growth equation

-   [ ] Add standard errors or a measure of confidence around each cumulative enhancement value
-   [ ] Solution for how to represent this error/confidence once we add up the total enhancement value across all species

### Notes

-   [ ] Some of the lifehistory values have been hard to validate (each resource we find gives different values for the same population) or obtain in consistent formats (some give ranges, some are total length (nose to tail tip) while others are carapace length, or disk length (for rays).

-   [ ] this is a work in progress and we have some contingency (eg. getting age/length at settlement (called recruitment), one year, maturity, and harvest, as they are all often referred to as 'recruitment' depending on the nature of the resource) in place for later discussions on choosing the best metric

-   [x] What is 'theoretical age when length is 0'? - This is a scaling parameter to standardise growth curves of different fish species and directly compare them. Can estimate using the von Bertalanffy growth model if you have length at various points in time, growth rate, and the asymptotic (i.e., maximum) length of a species.

`L(t) = L_inf * (1 - exp(-K*(t - t_0)))`

L_inf = asymptotic length; K = growth rate; t_0 = theoretical age at length 0
