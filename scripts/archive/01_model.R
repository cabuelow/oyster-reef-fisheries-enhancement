# model fisheries enhancement from restoration

# set up some initial params

dens <- 0.072 # juveniles per unit area enhanced by restoration
m <- 6.707877456 # annual mortality rate
t_max <- 1 # maximum age - 0.70958904
t_0 <- 0 # theoretical age when length is 0
t_harv <- 1 # age of recruitment to fishery
l_asym <- 55 # asymptotic size (length in cm)
Ks <- 3 # Brody growth coef 
a <- -0.958 # intercept of length-weight relationship
b <- 1.22533 # slope of length-weight relationship
years <- 16  # number of years since restoration

# set up as a dataframe

dat <- data.frame(species = 'Snapper', dens = dens, mf = NA,
                  m = m, t_max = t_max, t_0 = t_0, t_harv = t_harv,
                  l_asym = l_asym, Ks = Ks, a = a, b = b)

# function to return a dataframe of densities, lengths and weights in each year

mod_enhance <- function(dens, spp, mf, m, t_max, t_0, t_harv, l_asym, Ks, a, b, years){
  df <- data.frame(species = spp, male_female = mf, year = 0, d_ind_unit_area = 0, length_cm = 0, weight_g = 0, biomass_g_unit_area = 0, weight_g_i = 0, biomass_g_unit_area_i = 0, net_biomass_g_unit_area = 0)
  for(i in 1:years){
    df[i+1,1] <- spp # spp
    df[i+1,2] <- mf # male or female
    df[i+1,3] <- i # year post restoration
    df[i+1,4] <- if(i<=t_max)(dens*exp(-m*(i-0.5)))else(0) # density of recruited individuals in each year, all die after max age
    df[i+1,5] <- if(i<=t_max)(l_asym*(1-exp(-Ks*(i-t_0))))else(0) # estimate length using von bert eqn, only while fish are alive
    df[i+1,6] <- if(i<=t_max){a*df[i+1,5]^b}else(0) # convert length to weight, only if fish is alive
    df[i+1,7] <- if(i<=t_max){df[i+1,4]*df[i+1,6]}else(0) # convert to biomass 
    df[i+1,8] <- if(i>1 & i<=t_max)(df[i+1,6] - df[i,6])else(0) # get the incremental increase in weight for each time interval (i.e. year)
    df[i+1,9] <- if(i<=t_max){df[i+1,4]*df[i+1,8]}else(0) # convert to incremental biomass gain each year
    df[i+1,10] <- if(i==t_harv){df[i+1,7]}else if(i>t_harv){df[i,10]+df[i+1,9]}else(0) # cumulative incremental gains in biomass (net gains)
    }
  return(df)
}

# run the model

enhancement <- mod_enhance(dat$dens, dat$species, NA, dat$m, dat$t_max, dat$t_0, 
                           dat$t_harv, dat$l_asym, dat$Ks, dat$a, dat$b, years = years)


# have a look 

head(enhancement)
tail(enhancement)
plot(x = enhancement$year, y = enhancement$net_biomass_g_unit_area, col = 'red')

# special case for squid
# we only have the age-weight relationship for males and females
# they also are harvested at 0.5 years, and only live for 0.7 years
# so we will adapt function above so that we bypass the length estimation with von bert
# and the conversion of length to weight
# to instead just use max_weight and multiply by density to get biomass

# from Pecl 2000 thesis - table 2.2
# tmax = 0.70958904
# female weight in grams in first year
weight_max <- exp(-12.250)*((365*0.70958904)^(3.61)) # age-weight relationship follows power curve

# male weight in grams in first year
weight_max <- exp(-9.805)*((365*0.70958904)^(3.149)) # age-weight relationship follows power curve


dens <- 0.072 # juveniles per unit area enhanced by restoration
m <- 6.707877456 # annual mortality rate
t_max <- 1 # maximum age - 0.70958904
t_0 <- 0 # theoretical age when length is 0
t_harv <- 1 # age of recruitment to fishery
l_asym <- 55 # asymptotic size (length in cm)
Ks <- 3 # Brody growth coef 
years <- 16  # number of years since restoration

# set up as a dataframe

dat <- data.frame(species = 'Squid', dens = dens, mf = NA,
                  m = m, t_max = t_max, t_0 = t_0, t_harv = t_harv,
                  l_asym = l_asym, Ks = Ks, weight_max = weight_max)

# function to return a dataframe of densities, lengths and weights in each year

mod_enhance_squid <- function(dens, spp, mf, m, t_max, t_0, t_harv, l_asym, Ks, years, weight_max){
  df <- data.frame(species = spp, male_female = mf, year = 0, d_ind_unit_area = 0, length_cm = 0, weight_g = 0, biomass_g_unit_area = 0, weight_g_i = 0, biomass_g_unit_area_i = 0, net_biomass_g_unit_area = 0)
  for(i in 1:years){
    df[i+1,1] <- spp # spp
    df[i+1,2] <- mf # male or female
    df[i+1,3] <- i # year post restoration
    df[i+1,4] <- if(i<=t_max)(dens*exp(-m*(i-0.5)))else(0) # density of recruited individuals in each year, all die after max age
    df[i+1,5] <- NA # estimate length using von bert eqn, only while fish are alive, don't estimate for squid
    df[i+1,6] <- if(i<=t_max){weight_max}else(0) # use max weight in grams
    df[i+1,7] <- if(i<=t_max){df[i+1,4]*df[i+1,6]}else(0) # convert to biomass 
    df[i+1,8] <- if(i>1 & i<=t_max)(df[i+1,6] - df[i,6])else(0) # get the incremental increase in weight for each time interval (i.e. year)
    df[i+1,9] <- if(i<=t_max){df[i+1,4]*df[i+1,8]}else(0) # convert to incremental biomass gain each year
    df[i+1,10] <- if(i==t_harv){df[i+1,7]}else if(i>t_harv){df[i,10]+df[i+1,9]}else(0) # cumulative incremental gains in biomass (net gains)
  }
  return(df)
}

# run the model

enhancement <- mod_enhance_squid(dat$dens, dat$species, NA, dat$m, dat$t_max, dat$t_0, 
                           dat$t_harv, dat$l_asym, dat$Ks, years = years, weight_max = dat$weight_max)


# have a look 

head(enhancement)
tail(enhancement)
plot(x = enhancement$year, y = enhancement$net_biomass_g_unit_area, col = 'red')
