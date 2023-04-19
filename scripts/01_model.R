# model biomass enhanced by restored reefs based on juvenile abundance
# CABuelow

# set up some initial parameters (all are spp specific)
# TODO: what is t0?

dpost <- 50 # juveniles per hectare post restoration
dpre <- 30 # juveniles per hectare pre restoration
mort <- 0.1 # mortality
t_max <- 26 # maximum age
t_0 <- 0.1 # theoretical age when length is 0
t_harv <- 5 # age of recruitment to fishery
leng_asym <- 20 # asymptotic size (length in cm)
Ks <- 0.5 # Brody growth coef 
a <- 0.1 # intercept of length-weight relationship
b <- 0.5 # slope of length-weight relationship
area_restor <- 20 # area restored in hectares


