# model biomass enhanced by restored reefs based on juvenile abundance, 
# incorporating fish density uncertainty, modelled as a truncated normal distribution
# TODO: allow males and females to have different density estimates

library(msm)
library(tidyverse)
source('scripts/functions/helpers.R')
set.seed(123)

# load data

dat <- read.csv('data/fish-dat.csv')
area <- 1 # area restored in hectares
num_years <- 50 # number of years since restoration

# simulate fish densities from a normal distribution truncated  at 0 (n = 100000)
# representing sampling distribution of density differences (mean +- std. error)
# apply biomass enhancement model to the density distribution to estimate enhancement uncertainty

n <- 1000 # bump up down the road
spp <- unique(dat$species)
tmp <- list() # store results

for(i in seq_along(spp)){
  ind <- dat %>% filter(species == spp[i])
  if(nrow(ind) > 1){
    indM <- ind %>% filter(male_female == 'M')
    dens_distM <- sim_truncnorm(n, indM$d_diff, indM$d_diff_se, lower = 0, upper = Inf)
    if(mean(dens_distM) != indM$d_diff){print(paste0('warning: Male densities are different ', mean(dens_distM) - indM$d_diff))}
    if(sd(dens_distM) != indM$d_diff_se){print(paste0('warning: Male std. errs are different by ', sd(dens_distM) - indM$d_diff_se))}
    enhanM <- lapply(dens_distM/2, mod_enhance, spp = indM$species, mf = indM$male_female, m = indM$m, t_max = indM$t_max, t_0 = indM$t_0, 
                          t_harv = indM$t_harv, l_asym = indM$l_asym, Ks = indM$Ks, a = indM$a, b = indM$b, area_restor = area, years = num_years)
    indF <- ind %>% filter(male_female == 'F')
    dens_distF <- sim_truncnorm(n, indF$d_diff, indF$d_diff_se, lower = 0, upper = Inf)
    if(mean(dens_distF) != indF$d_diff){print(paste0('warning: Female densities are different ', mean(dens_distF) - indF$d_diff))}
    if(sd(dens_distF) != indF$d_diff_se){print(paste0('warning: Female std. errs are different by ', sd(dens_distF) - indF$d_diff_se))}
    enhanF <- lapply(dens_distF/2, mod_enhance, spp = indF$species, mf = indF$male_female, m = indF$m, t_max = indF$t_max, t_0 = indF$t_0, 
                     t_harv = indF$t_harv, l_asym = indF$l_asym, Ks = indF$Ks, a = indF$a, b = indF$b, area_restor = area, years = num_years)
    tmp[[i]] <- rbind(data.frame(sim = rep(1:n, each = num_years), do.call(rbind, enhanM)), data.frame(sim = rep(1:n, each = num_years), do.call(rbind, enhanF)))
  }else{
    dens_dist <- sim_truncnorm(n, ind$d_diff, ind$d_diff_se, lower = 0, upper = Inf)
    if(mean(dens_dist) != ind$d_diff){print(paste0('warning: densities are different ', mean(dens_dist) - ind$d_diff))}
    if(sd(dens_dist) != ind$d_diff_se){print(paste0('warning: std. errs are different by ', sd(dens_dist) - ind$d_diff_se))}
    enhan <- lapply(dens_dist, mod_enhance, spp = ind$species, mf = ind$male_female, m = ind$m, t_max = ind$t_max, t_0 = ind$t_0, 
                     t_harv = ind$t_harv, l_asym = ind$l_asym, Ks = ind$Ks, a = ind$a, b = ind$b, area_restor = area, years = num_years)
    tmp[[i]] <- data.frame(sim = rep(1:n, each = num_years), do.call(rbind, enhan))
  }
}

results <- do.call(rbind, tmp)
write.csv(results, 'outputs/biomass-enhancement.csv', row.names = F)


