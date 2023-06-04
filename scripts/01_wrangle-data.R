# wrangle and join life history parameters and juvenile density enhancements

library(tidyverse)

# life history parameters

dat <- read.csv('data/fish-dat.csv') %>% 
  mutate(m = ifelse(is.na(use_m), m_Then2014, m)) %>% # here use literature 'm' values when confident, otherwise use Then eqn
  mutate(t_harv = ifelse(harvested == 'y', t_harv, t_maturity)) %>% # for species that aren't harvested, setting their t_harv value to age at maturity instead
  mutate(t_harv = ifelse(t_harv < 1, 1, t_harv)) %>% # if time of harvest or maturity is less than 1 year, set it to first year (otherwise can model at finer resolution)
  pivot_longer(cols = c(m, m_Then2014), names_to = 'mortality', values_to = 'mortality_val') %>% 
  data.frame()

# juvenile densities

dat_density <- read.csv('data/juvenile_densities_20230529.csv') %>% 
  pivot_wider(id_cols = c(Site, common_name), names_from = 'Habitat_agg', 
              values_from = c('densitymean', 'densityvar', 'densitystd', 'n')) %>% 
  mutate(d = densitymean_Reef - densitymean_Unstructured) %>% # calculate density enhancement
  filter(d > 0) %>% # filter for only positive density enhancements
  #mutate(d_se = (sqrt((densityvar_Reef*n_y_Reef)+(densityvar_Unstructured*n_y_Unstructured))/(n_y_Reef+n_y_Unstructured))/sqrt((n_y_Reef+n_y_Unstructured)/2)) %>% # calculate standard error via weighted average of variances
  mutate(d_se = densitystd_Reef/sqrt(n_Reef)) %>% # all of the densities on unstructured are 0, so will just use standard error of reef
  data.frame()

# join to other data frame

dat_final <- dat_density %>% 
  select(Site, common_name, d, d_se) %>% 
  rename(d_site = 'Site', species = 'common_name') %>% 
  left_join(dat, by = 'species')

write.csv(dat_final, 'data/wrangled-dat.csv', row.names = F)

