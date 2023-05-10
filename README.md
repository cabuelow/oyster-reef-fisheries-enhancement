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
  
  - [x] von Bert growth eqn may not be appropriate for all species, in which case switch between that and a different one, e.g., Schnute. Will just use von Bert for now (see note from DH below).

### Notes from DH

-   keep a close eye on the maximum length (L infinity) in any fitted growth curve. If this is way off then you may get some non-trivial overestimates. Rod or any fisheries people in the lab should be able to tell whether the estimate is reasonable.

- VB curves fit pretty well for most species and are still by far the most common in the literature. If a particular study found a better fitting function, then I’d use that, but I mostly see this as something to be aware of. I may have overstated the importance of this a little in our conversation. An encouraging thing about our analysis is that we’re most interested in fish from the recruitment age/size through to a bit less than their maximum age/size (few ever reach it because of fishing mortality at earlier ages/sizes). It also happens that most data used to fit growth curves is from fish within this range too, so the region where we want the curve to fit well is usually where it has the most data.
