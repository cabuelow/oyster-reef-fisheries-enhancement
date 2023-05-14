# model biomass enhanced by restored reefs based on juvenile abundance
# CABuelow

# set up some initial parameters (all are spp specific)

dpost <- 50 # juveniles per hectare post restoration
dpre <- 30 # juveniles per hectare pre restoration
m <- 0.1 # annual mortality rate
t_max <- 26 # maximum age
t_0 <- 0.1 # theoretical age when length is 0
t_harv <- 5 # age of recruitment to fishery
l_asym <- 20 # asymptotic size (length in cm)
Ks <- 0.5 # Brody growth coef 
a <- 0.1 # intercept of length-weight relationship
b <- 0.5 # slope of length-weight relationship
area_restor <- 20 # area restored in hectares
years <- 50 # number of years since restoration

# set up as a dataframe

dat <- data.frame(species = 'Snapper', dpost = dpost, dpre = dpre,
                 m = m, t_max = t_max, t_0 = t_0, t_harv = t_harv,
                 l_asym = l_asym, Ks = Ks, a = a, b = b)
#write.csv(dat, 'data/template-df.csv', row.names = F)

# function to return a dataframe of densities, lengths and weights in each year

mod_enhance <- function(spp, dpost, dpre, m, t_max, t_0, t_harv, l_asym, Ks, a, b, area_restor, years){
  df <- data.frame(species = NA, year = NA, denhance = NA, length = NA, weight = NA, weight_i = NA, benhance = NA, cumul_benhance = NA)
  for(i in 1:years){
  df[i,1] <- spp # spp
  df[i,2] <- i # year
  df[i,3] <- (dpost-dpre)*exp(-m*(i-0.5)) # estimate biomass enhancement
  df[i,4] <- l_asym*(1-exp(-Ks*(i-t_0))) # estimate length using von bert eqn
  df[i,5] <- a*df[i,4]^b # convert length to weight
  df[i,6] <- if(i>1){df[i,5]-df[i-1,5]}else(0) # get the difference in weight for each time interval (i.e. year)
  }
  for(i in t_harv:years){
  df[i,7] <- df[t_harv, 'weight'] + (sum(df[t_harv:t_max, 'weight_i'], na.rm = T))*df[i,3]*area_restor
  df[i,8] <- if(i>t_harv){df[i-1,8]+df[i,7]}else(df[i,7]) 
  }
  return(df)
}

# run the model

enhancement <- mod_enhance(dat$species, dat$dpost, dat$dpre, dat$m, dat$t_max, dat$t_0, 
           dat$t_harv, dat$l_asym, dat$Ks, dat$a, dat$b, 
           area_restor = area_restor, years = years)

# have a look 

head(enhancement)
plot(x = enhancement$year, y = enhancement$cumul_benhance)

#TODO - will need an estimate of variance around cumul_benhance
