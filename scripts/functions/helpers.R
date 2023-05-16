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

