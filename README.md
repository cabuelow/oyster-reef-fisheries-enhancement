## Modelling fisheries enhancement from restored oyster reefs

This repository provides code to estimate fisheries enhancement from restored oyster reefs.

### To consider

- [ ] Currently just calculating biomass enhancement from the year a fish recruits to fishery (i.e is harvested). Could alternatively do biomass enhancement from juvenile recruitment (i.e. age 1)
- [ ] Where have different parameter estimates for males and females, should we:
    - 1. take an average of the life history parameters?
    - 2. simulate separately for males and females and add up enhancement at the end
    - Probably only makes a difference if we have density differences...?
    - For now dividing densities by 2 if they are the same, and modelling separately
- [ ] gross vs. net production 
  - Gross biomass enhancement: includes biomass gains by both living individuals and individuals that die in the intervening period
  - Net biomass enhancement: only considers biomass gains by individuals that survives to maximum age using incremental weight gains
  - We will use net biomass enhancement to be similar to Tomago case study
  
### TODO

- [X] Sensitivity analysis of key life history parameters
- have done for mortality

-   [X] Add standard errors or a measure of confidence around each cumulative enhancement value
  - zu Ermgassen 2016 constructs uncertainty intervals using the mean and std. error of juvenile densities. assumes all other LH paramaters are invariant (b/c don't have data on how they vary..perhaps we can do better). But can do this for all of the parameter estimates. Or do sensitivity test if just have a few.

-   [X] Solution for how to represent this error/confidence once we add up the total enhancement value across all species
  - If we assume the densities of fish are independent, can just sum variances across species once have estimated using simulation for each one (step above) because samples sizes are the same
  - Otherwise use covariance in density among fish and use law of total variance (covariance)
  
  - [x] von Bert growth eqn may not be appropriate for all species, in which case switch between that and a different one, e.g., Schnute. Will just use von Bert for now (see note from DH below).

### Notes from DH

-   keep a close eye on the maximum length (L infinity) in any fitted growth curve. If this is way off then you may get some non-trivial overestimates. Rod or any fisheries people in the lab should be able to tell whether the estimate is reasonable.

- VB curves fit pretty well for most species and are still by far the most common in the literature. If a particular study found a better fitting function, then I’d use that, but I mostly see this as something to be aware of. I may have overstated the importance of this a little in our conversation. An encouraging thing about our analysis is that we’re most interested in fish from the recruitment age/size through to a bit less than their maximum age/size (few ever reach it because of fishing mortality at earlier ages/sizes). It also happens that most data used to fit growth curves is from fish within this range too, so the region where we want the curve to fit well is usually where it has the most data.
