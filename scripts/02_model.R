# model biomass enhanced by restored reefs based on juvenile density, 
# incorporating density uncertainty, modelled as a truncated normal distribution
# currently assuming a 1:1 M:F ratio and dividing densities by 2

library(msm)
library(tidyverse)
source('scripts/functions/helpers.R')
set.seed(123)

# load data

dat <- read.csv('data/wrangled-dat.csv')
num_years <- max(dat$t_max) # model through to maximum lifespan of species
mort_iter <- unique(dat$mortality) # mortality to iterate over for sensitivity analysis

# simulate fish densities from a normal distribution truncated  at 0 (n = 100000)
# representing sampling distribution of density differences (mean +- std. error)
# apply biomass enhancement model to the density distribution to estimate enhancement uncertainty
# note that densities are provided as meanCount/m2 (average number of individuals/100m2)

n <- 10000 # bump up down the road
spp <- unique(dat$species)
tmp <- list()

# model individual species at each location, 
# iterating through different mortality estimates for sensitivity analysis
# and for males vs. females

system.time(
for(i in seq_along(mort_iter)){
  dat2 <- dat %>% filter(mortality == mort_iter[i])
  tmp2 <- list()
  for(j in seq_along(spp)){
    indspp <- dat2 %>% filter(species == spp[j])
    sites <- unique(indspp$d_site)
    tmp3 <- list()
    for(k in seq_along(unique(indspp$d_site))){
      ind <- indspp %>% filter(d_site == sites[k])
      if(ind$species[1] != 'Southern squid'){
      if(length(unique(ind$male_female)) > 1){
        indM <- ind %>% filter(male_female == 'M')
        dens_dist <- sim_truncnorm(n, indM$d, indM$d_se, lower = 0, upper = Inf)
        if(mean(dens_dist) != indM$d){print(paste0('warning: densities are different ', mean(dens_dist) - indM$d))}
        if(sd(dens_dist) != indM$d_se){print(paste0('warning: std. errs are different by ', sd(dens_dist) - indM$d_se))}
        enhanM <- lapply(dens_dist/2, mod_enhance, spp = indM$species, mf = indM$male_female, m = indM$mortality_val, t_max = indM$t_max, t_0 = indM$t_0, 
                         t_harv = indM$t_harv, l_asym = indM$l_asym, Ks = indM$Ks, a = indM$a, b = indM$b, years = num_years)
        indF <- ind %>% filter(male_female == 'F')
        enhanF <- lapply(dens_dist/2, mod_enhance, spp = indF$species, mf = indF$male_female, m = indF$mortality_val, t_max = indF$t_max, t_0 = indF$t_0, 
                         t_harv = indF$t_harv, l_asym = indF$l_asym, Ks = indF$Ks, a = indF$a, b = indF$b, years = num_years)
        tmp3[[k]] <- rbind(data.frame(sim = rep(1:n, each = num_years+1), site = indM$d_site, do.call(rbind, enhanM)), data.frame(sim = rep(1:n, each = num_years+1), site = indF$d_site, do.call(rbind, enhanF)))
      }else{
        dens_dist <- sim_truncnorm(n, ind$d, ind$d_se, lower = 0, upper = Inf)
        if(mean(dens_dist) != ind$d){print(paste0('warning: densities are different ', mean(dens_dist) - ind$d))}
        if(sd(dens_dist) != ind$d_se){print(paste0('warning: std. errs are different by ', sd(dens_dist) - ind$d_se))}
        enhan <- lapply(dens_dist, mod_enhance, spp = ind$species, mf = ind$male_female, m = ind$mortality_val, t_max = ind$t_max, t_0 = ind$t_0, 
                        t_harv = ind$t_harv, l_asym = ind$l_asym, Ks = ind$Ks, a = ind$a, b = ind$b, years = num_years)
        tmp3[[k]] <- data.frame(sim = rep(1:n, each = num_years+1), site = ind$d_site, do.call(rbind, enhan))
      }
      }else{
        indM <- ind %>% filter(male_female == 'M')
        dens_dist <- sim_truncnorm(n, indM$d, indM$d_se, lower = 0, upper = Inf)
        if(mean(dens_dist) != indM$d){print(paste0('warning: densities are different ', mean(dens_dist) - indM$d))}
        if(sd(dens_dist) != indM$d_se){print(paste0('warning: std. errs are different by ', sd(dens_dist) - indM$d_se))}
        enhanM <- lapply(dens_dist/2, mod_enhance_squid, spp = indM$species, mf = indM$male_female, m = indM$mortality_val, t_max = indM$t_max, t_0 = indM$t_0, 
                         t_harv = indM$t_harv, l_asym = indM$l_asym, Ks = indM$Ks, years = num_years, weight_max = indM$weight_max)
        indF <- ind %>% filter(male_female == 'F')
        enhanF <- lapply(dens_dist/2, mod_enhance_squid, spp = indF$species, mf = indF$male_female, m = indF$mortality_val, t_max = indF$t_max, t_0 = indF$t_0, 
                         t_harv = indF$t_harv, l_asym = indF$l_asym, Ks = indF$Ks, years = num_years, weight_max = indF$weight_max)
        tmp3[[k]] <- rbind(data.frame(sim = rep(1:n, each = num_years+1), site = indM$d_site, do.call(rbind, enhanM)), data.frame(sim = rep(1:n, each = num_years+1), site = indF$d_site, do.call(rbind, enhanF)))
      }
    }
    tmp2[[j]] <- do.call(rbind, tmp3)
  }
  tmp[[i]] <- data.frame(m = mort_iter[i], do.call(rbind, tmp2))
}
)

results <- do.call(rbind, tmp)

write.csv(results, 'outputs/biomass-enhancement.csv', row.names = F)


