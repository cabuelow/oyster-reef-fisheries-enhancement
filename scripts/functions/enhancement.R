enhancement <- function(species_name, 
                        common_name, 
                        initial_density, 
                        mortality_rate, 
                        max_age, 
                        recruitment_age, 
                        L_inf, 
                        k, 
                        t0, 
                        a, 
                        b,
                        delta_area,
                        swept_area){
  
  
  if(initial_density > 0){
    # standardize the initial density by the area swept by the seine
    initial_density <- initial_density * swept_area 
    
    # vector to store results
    density <- numeric(max_age)
    
    # initialize with the density at the first time step
    #density[1] <- initial_density
    
    # initialize with the length in the first age class
    length <- numeric(max_age)
    
      # VBGF
    length[1] <- L_inf * (1 - exp(-k * (1 - t0))) 
    
    # Schnute
    # length[i] <- L4 + ((L4 - L1) * (1 - exp(a(1 - 1))/(1 - exp(a(4 - 1))))) ^ (1 / b)
    
    # if no mortality estimate is available use the estimator from https://doi.org/10.1093/icesjms/fsu136 based on max_age
    if(is.na(mortality_rate)){
      mortality_rate <- 4.899 * max_age ^ -0.916
    }
    
    # for every year until the maximum age of the species
    for (i in 1:max_age) {
      
      # calculate the density
      if(i == 1){
        density[i] <- initial_density * exp(-mortality_rate * (i - 0.5))
      } else {
        density[i] <- density[i-1] * exp(-mortality_rate * (i - 0.5))
      }
      
      # calculate average length for each age class
      # VBGF
      length[i] <- L_inf * (1 - exp(-k * (i - t0)))
    }
    
    # calculate the weights
    weight <- (a * length ** b)
    
    # calculate incremental increase in weight
    delta_weight <- weight - lag(weight)
    
    # the first fully recruited year class is the total weight
    delta_weight[1] <- weight[1]
    delta_weight[recruitment_age] <- weight[recruitment_age]
    
    # calculate the biomass
    biomass_ha <- density * delta_weight 
    
    # calculate the increas
    biomass_increase <- biomass_ha * delta_area
    
    # put it all in a dataframe
    result <- bind_cols(species_name = species_name,
                        common_name = common_name,
                        initial_density = initial_density,
                        mortality = mortality_rate,
                        year_class = 1:max_age, #recruitment_age:max_age, 
                        recruitment_age = recruitment_age,
                        density = density, 
                        average_length = length, 
                        average_weight = weight,
                        delta_weight = delta_weight,
                        biomass_ha = biomass_ha,
                        biomass_increase = biomass_increase)
    
    # return the result
    return(result)
  }
}
