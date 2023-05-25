# summarise results and plot

library(tidyverse)
library(patchwork)
library(RColorBrewer)

# calculate mean, standard deviation, upper and lower quartiles of biomass enhancement for each species in each year

dat <- read.csv('outputs/biomass-enhancement.csv') 
dat_param <- read.csv('data/fish-dat.csv') %>% 
  select(species, harvested) %>% distinct() %>% 
  mutate(harvested = recode(harvested, 'y' = 'Harvested species', 'n' = 'Non-harvested species'))

dat2 <- dat %>% 
  left_join(select(dat_param, species, harvested), by = 'species') %>% 
  mutate(net_biomass_kg_ha = (net_biomass_g_100m2/100)*10) %>% # convert biomass to kg/ha
  filter(m == 'm') %>% # using selected 'm' values for main results
  group_by(sim, species, harvested, site, year) %>% 
  summarise(net_biomass_kg_ha = sum(net_biomass_kg_ha)) %>%  # here summing by gender 
  group_by(species, harvested, site, year) %>% 
  summarise(net_biomass_kg_ha_mean = mean(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_var = var(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_sd = sd(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_se = sd(net_biomass_kg_ha, na.rm = T)/sqrt(max(dat$sim)),
            net_biomass_kg_ha_95 = (sd(net_biomass_kg_ha, na.rm = T)/sqrt(max(dat$sim)))*1.96,
            net_biomass_kg_ha_upp = quantile(net_biomass_kg_ha, 0.75, na.rm = T),
            net_biomass_kg_ha_low = quantile(net_biomass_kg_ha, 0.25, na.rm = T))

# total biomass enhancement across all species
# have same sample size for each species in each year, so don't need to weight variance or biomass estimates by sample size

a <- dat2 %>% 
  group_by(year) %>% 
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_sd = sqrt(sum(net_biomass_kg_ha_var))) %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_enhance - net_sd, 
                  ymax = net_enhance  + net_sd), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = net_enhance), col = 'red') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  ggtitle('A) All species (+- SD)') +
  xlab('Year') +
  #ylim(c(0,1500)) +
  theme_classic()
a

a2 <- dat2 %>% 
  group_by(harvested, year) %>% 
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_sd = sqrt(sum(net_biomass_kg_ha_var))) %>%
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_enhance - net_sd, 
                  ymax = net_enhance  + net_sd), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = net_enhance), col = 'red') +
  facet_wrap(~harvested, scales = 'free_y') +
  ylab('') +
  ggtitle('B) All species (+- SD): Havested vs. non-harvested') +
  xlab('Year') +
  #ylim(c(0,1500)) +
  theme_classic()
a2 

# species with uncertainty

b <- dat2 %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_biomass_kg_ha_mean - net_biomass_kg_ha_sd, 
                  ymax = net_biomass_kg_ha_mean + net_biomass_kg_ha_sd, group = species), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = net_biomass_kg_ha_mean, col = species)) +
  ylab('') +
  xlab('Year') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  ggtitle('C) Individual species (+- SD) at each site') +
  facet_wrap(~site, scales = 'free_y') +
  #ylim(c(0,80)) +
  theme_classic() +
  theme(legend.title = element_blank()) 
b

# plot by species

c <- dat2 %>% 
  ggplot() +
  geom_area(aes(x = year, y = net_biomass_kg_ha_mean, fill = factor(species)), position = 'stack') +
  ylab('') +
  xlab('Year') +
  ggtitle('D) Individual species (cumulative) at each site') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  facet_wrap(~site, scales = 'free_y') +
  #ylim(c(0,80)) +
  theme_classic() +
  theme(legend.title = element_blank()) 
c

# plot together

d <- a+a2+ plot_layout(nrow = 1, widths = c(0.5, 1))
g <- d/b/c
g

ggsave('outputs/bioenhancement.png', width = 10, height = 10)

# plot sensitivity to m, total and species

e <- dat %>% 
  filter(year == 25) %>% # pick max year where all species enm2ncements will be stable
  group_by(m, sim, species) %>% 
  summarise(net_biomass_g_m2 = sum(net_biomass_g_m2)) %>% # sum by gender
  ggplot() +
  aes(x = m, y = net_biomass_g_m2) +
  geom_jitter(alpm2 = 0.1, size = 0.2, width = 0.1) +
  geom_violin(fill = 'transparent', col = 'cyan') +
  ggtitle('A) All species') +
  ylab(bquote('Biomass enm2ncement (g ' ~m2^-1~year^-1*')')) +
  xlab('') +
  theme_classic()
e  

f <- dat %>% 
  filter(year == 25) %>% # pick max year where all species enm2ncements will be stable
  group_by(m, sim, species) %>% 
  summarise(net_biomass_g_m2 = sum(net_biomass_g_m2)) %>% # sum by gender
  ggplot() +
  aes(x = m, y = net_biomass_g_m2) +
  geom_jitter(alpm2 = 0.1, size = 0.2, width = 0.1) +
  geom_violin(fill = 'transparent', col = 'cyan') +
  facet_grid(~species) +
  ggtitle('B) Individual species') +
  ylab('') +
  xlab('')+
  theme_classic()
f  

h <- e+f+plot_layout(widths = c(0.2,1))
h

ggsave('outputs/bioenm2ncement_mortality-sensitivity.png', width = 12, height = 4)

# plot everything

g/h

ggsave('outputs/bioenm2ncement_plots_all.png', width = 12, height = 8)

