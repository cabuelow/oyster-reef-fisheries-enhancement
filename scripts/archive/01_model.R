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
  df <- data.frame(species = NA, male_female = NA, year = NA, d_ind_ha = NA, length_cm = NA, weight_g = NA, weight_g_i = NA, bio_enhance_g_ha = NA, cumul_bio_enhance_g_ha = NA)
  for(i in 1:years){
    df[i,1] <- spp # spp
    df[i,2] <- mf # male or female
    df[i,3] <- i # year post restoration
    df[i,4] <- if(i<=t_max)(dens*exp(-m*(i-0.5)))else(0) # density of recruited individuals in each year, all die after max age
    df[i,5] <- if(i<=t_max)(l_asym*(1-exp(-Ks*(i-t_0))))else(0) # estimate length using von bert eqn, only while fish are alive
    df[i,6] <- if(i<=t_max){a*df[i,5]^b}else(0) # convert length to weight, only if fish is alive
  }
  for(i in 1:years){
    df[i,7] <- if(i<t_max)(df[i+1,6] - df[i,6])else(0) # get the incremental increase in weight for each time interval (i.e. year)
  }
  for(i in t_harv:years){
    df[i,8] <- (df[t_harv, 'weight_g'] + (sum(df[t_harv:i, 'weight_g_i'], na.rm = T)))*df[i,4]
    df[i,9] <- if(i==t_harv){df[i,8]}else{df[i-1,9]+df[i,8]}
  }
  return(df)
}

# run the model

enhancement <- mod_enhance(dat$dens, dat$species, NA, dat$m, dat$t_max, dat$t_0, 
                           dat$t_harv, dat$l_asym, dat$Ks, dat$a, dat$b, 
                           area_restor = area_restor, years = years)

# have a look 

head(enhancement)
plot(x = enhancement$year, y = enhancement$cumul_bio_enhance_g_ha)

