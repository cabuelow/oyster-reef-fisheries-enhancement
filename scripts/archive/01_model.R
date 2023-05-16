# model fisheries enhancement from restoration

# set up some initial params

dens <- 0.415 # juveniles per hectare post restoration
m <- 0.78 # annual mortality rate
t_max <- 7 # maximum age
t_0 <- -1.1 # theoretical age when length is 0
t_harv <- 1 # age of recruitment to fishery
l_asym <- 22 # asymptotic size (length in cm)
Ks <- 0.33 # Brody growth coef 
a <- 0.0316 # intercept of length-weight relationship
b <- 3.03 # slope of length-weight relationship
area_restor <- 1 # area restored in hectares
years <- 25 # number of years since restoration

# set up as a dataframe

dat <- data.frame(species = 'Snapper', dens = dens,
                  m = m, t_max = t_max, t_0 = t_0, t_harv = t_harv,
                  l_asym = l_asym, Ks = Ks, a = a, b = b)
#write.csv(dat, 'data/template-df.csv', row.names = F)

# function to return a dataframe of densities, lengths and weights in each year

mod_enhance <- function(dens, spp, mf, m, t_max, t_0, t_harv, l_asym, Ks, a, b, area_restor, years){
  df <- data.frame(species = NA, male_female = NA, year = NA, denhance = NA, length = NA, weight = NA, weight_i = NA, biomass = NA, biomass_i = NA, benhance = NA)
  for(i in 1:years){
    df[i,1] <- spp # spp
    df[i,2] <- mf # male or female
    df[i,3] <- i # year
    df[i,4] <- dens*exp(-m*(i-0.5))
    df[i,5] <- l_asym*(1-exp(-Ks*(i-t_0))) # estimate length using von bert eqn
    df[i,6] <- if(i<=t_max){a*df[i,5]^b}else(df[t_max,6]) # convert length to weight, only if fish is alive
  }
  for(i in 1:years){
    df[i,7] <- df[i+1,6] - df[i,6] # get the incremental increase in weight for each time interval (i.e. year)
    df[i,8] <- df[i,4]*df[i,6] # convert weight to biomass
    df[i,9] <- df[i,4]*df[i,7] # convert weight to biomass
  }
  for(i in t_harv:years){
    df[i,10] <- if(i>t_max){(df[t_harv, 'biomass'] + (sum(df[t_harv:t_max, 'biomass_i'], na.rm = T)))*area_restor}else((df[t_harv, 'biomass'] + (sum(df[t_harv:i, 'biomass_i'], na.rm = T)))*area_restor)
  }
  return(df)
}

# run the model

enhancement <- mod_enhance(dat$dens, dat$species, NA, dat$m, dat$t_max, dat$t_0, 
                           dat$t_harv, dat$l_asym, dat$Ks, dat$a, dat$b, 
                           area_restor = area_restor, years = years)

# have a look 

head(enhancement)
plot(x = enhancement$year, y = enhancement$benhance)

