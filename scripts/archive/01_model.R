# model fisheries enhancement from restoration

# set up some initial params

dens <- 0.415 # juveniles per hectare enhanced by restoration
m <- 0.78 # annual mortality rate
t_max <- 7 # maximum age
t_0 <- -1.1 # theoretical age when length is 0
t_harv <- 2 # age of recruitment to fishery
l_asym <- 22 # asymptotic size (length in cm)
Ks <- 0.33 # Brody growth coef 
a <- 0.0316 # intercept of length-weight relationship
b <- 3.03 # slope of length-weight relationship
years <- 25 # number of years since restoration

# set up as a dataframe

dat <- data.frame(species = 'Snapper', dens = dens,
                  m = m, t_max = t_max, t_0 = t_0, t_harv = t_harv,
                  l_asym = l_asym, Ks = Ks, a = a, b = b)

# function to return a dataframe of densities, lengths and weights in each year

mod_enhance <- function(dens, spp, mf, m, t_max, t_0, t_harv, l_asym, Ks, a, b, area_restor, years){
  df <- data.frame(species = NA, male_female = NA, year = NA, d_ind_ha = NA, length_cm = NA, weight_g = NA, biomass_g_ha = NA, gross_biomass_g_ha = NA, weight_g_i = NA, biomass_g_ha_i = NA, net_biomass_g_ha = NA)
  for(i in 1:years){
    df[i,1] <- spp # spp
    df[i,2] <- mf # male or female
    df[i,3] <- i # year post restoration
    df[i,4] <- if(i<=t_max)(dens*exp(-m*(i-0.5)))else(0) # density of recruited individuals in each year, all die after max age
    df[i,5] <- if(i<=t_max)(l_asym*(1-exp(-Ks*(i-t_0))))else(0) # estimate length using von bert eqn, only while fish are alive
    df[i,6] <- if(i<=t_max){a*df[i,5]^b}else(0) # convert length to weight, only if fish is alive
    df[i,7] <- if(i<=t_max){df[i,4]*df[i,6]}else(0) # convert to biomass 
    df[i,8] <- if(i==t_harv){df[i,7]}else if(i>t_harv){df[i-1,8]+df[i,7]}else(0) # cumulative biomass gains over time (gross gains)
    df[i,9] <- if(i>1 & i<=t_max)(df[i,6] - df[i-1,6])else(0) # get the incremental increase in weight for each time interval (i.e. year)
    df[i,10] <- if(i<=t_max){df[i,4]*df[i,9]}else(0) # convert to incremental biomass gain each year
    df[i,11] <- if(i==t_harv){df[i,7]}else if(i>t_harv){df[i-1,11]+df[i,10]}else(0) # cumulative incremental gains in biomass (net gains)
    }
  return(df)
}

# run the model

enhancement <- mod_enhance(dat$dens, dat$species, NA, dat$m, dat$t_max, dat$t_0, 
                           dat$t_harv, dat$l_asym, dat$Ks, dat$a, dat$b, 
                           area_restor = area_restor, years = years)


# have a look 

head(enhancement)
plot(x = enhancement$year, y = enhancement$gross_biomass_g_ha, col = 'red')
points(x = enhancement$year, y = enhancement$net_biomass_g_ha)
