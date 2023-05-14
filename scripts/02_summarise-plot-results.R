# summarise results and plot

library(tidyverse)

# calculate mean, standard deviation, upper and lower quartiles of biomass enhancement for each species in each year

dat <- read.csv('outputs/biomass-enhancement.csv') 

dat2 <- dat %>% 
  filter(species != "Smooth toadfish") %>% 
  group_by(sim, species, year) %>% 
  summarise(denhance = sum(denhance, na.rm = T), # here summing by gender
            length = mean(length, na.rm = T),
            weight = sum(weight, na.rm = T),
            weight_i = sum(weight, na.rm = T),
            benhance = sum(benhance, na.rm = T),
            cumul_benhance = sum(cumul_benhance, na.rm = T)) %>% 
  group_by(species, year) %>% 
  summarise(cumul_benhance_mean = mean(cumul_benhance, na.rm = T),
            cumul_benhance_var = var(cumul_benhance, na.rm = T),
            cumul_benhance_sd = sd(cumul_benhance, na.rm = T),
            cumul_benhance_se = sd(cumul_benhance, na.rm = T)/sqrt(max(dat$sim)),
            cumul_benhance_95 = (sd(cumul_benhance, na.rm = T)/sqrt(max(dat$sim)))*1.96,
            cumul_benhance_upp = quantile(cumul_benhance, 0.75, na.rm = T),
            cumul_benhance_low = quantile(cumul_benhance, 0.25, na.rm = T)) %>% 
  data.frame()
            
# plot by species

dat2 %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = cumul_benhance_mean - cumul_benhance_sd, 
                  ymax = cumul_benhance_mean + cumul_benhance_sd, group = species), fill = "grey", alpha = 0.5) +
  geom_area(aes(x = year, y = cumul_benhance_mean, fill = factor(species)), position = 'stack') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  xlab('Year') +
  theme_classic() +
  theme(legend.title = element_blank())

# with uncertainty

dat2 %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = cumul_benhance_mean - cumul_benhance_sd, 
                 ymax = cumul_benhance_mean + cumul_benhance_sd, group = species), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = cumul_benhance_mean, col = species)) +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  xlab('Year') +
  theme_classic()


