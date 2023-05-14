# summarise results and plot
# TODO: need to fix model

library(tidyverse)

# calculate mean, standard deviation, upper and lower quartiles of biomass enhancement for each species in each year

dat <- read.csv('outputs/biomass-enhancement.csv') %>% 
  filter(species == 'Bridled Leatherjacket') %>% 
  group_by(species, male_female, year) %>% 
  summarise(cumul_benhance_mean = mean(cumul_benhance, na.rm = T),
            cumul_benhance_var = var(cumul_benhance, na.rm = T),
            cumul_benhance_sd = sd(cumul_benhance, na.rm = T),
            cumul_benhance_upp = quantile(cumul_benhance, 0.75, na.rm = T),
            cumul_benhance_low = quantile(cumul_benhance, 0.25, na.rm = T)) %>% 
  data.frame()
            
# plot

dat %>% 
  #filter(!is.na(cumul_benhance_sd)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = cumul_benhance_mean, col = species)) +
  geom_ribbon(aes(x = year, ymin = cumul_benhance_mean - cumul_benhance_sd, 
                  ymax = cumul_benhance_mean + cumul_benhance_sd), fill = "grey") +
  theme_classic()
