sim_truncnorm <- function(n, mean, sd, lower, upper){
  data <- msm::rtnorm(1, lower=((lower - mean)/sd), upper=((upper - mean)/sd))
  while (length(data) < n) {
    sample <- msm::rtnorm(1, lower=((lower - mean)/sd), upper=((upper - mean)/sd))
    data_copy = c(data, sample)
    data_copy_scaled = mean + sd * scale(data_copy)
    if (min(data_copy_scaled) >= lower & max(data_copy_scaled) <= upper) {
      data = c(data, sample)
    }}
  scaled_data = as.numeric(mean + sd * scale(data))
  return(scaled_data)
}

mod_enhance <- function(dens, spp, mf, m, t_max, t_0, t_harv, l_asym, Ks, a, b, area_restor, years){
  df <- data.frame(species = NA, male_female = NA, year = NA, denhance = NA, length = NA, weight = NA, weight_i = NA, benhance = NA, cumul_benhance = NA)
  for(i in 1:years){
    df[i,1] <- spp # spp
    df[i,2] <- mf # male or female
    df[i,3] <- i # year
    df[i,4] <- dens*exp(-m*(i-0.5)) # estimate juvenile density surviving to year i
    df[i,5] <- l_asym*(1-exp(-Ks*(i-t_0))) # estimate length using von bert eqn
    df[i,6] <- a*df[i,4]^b # convert length to weight
    df[i,7] <- if(i>1){df[i,5]-df[i-1,5]}else(0) # get the incremental increase in weight for each time interval (i.e. year)
  }
  for(i in t_harv:years){
    df[i,8] <- df[t_harv, 'weight'] + (sum(df[t_harv:t_max, 'weight_i'], na.rm = T))*df[i,3]*area_restor
    df[i,9] <- if(i>t_harv){df[i-1,8]+df[i,7]}else(df[i,7]) 
  }
  return(df)
}
