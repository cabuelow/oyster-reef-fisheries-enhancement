# summarise results and plot

library(tidyverse)
library(patchwork)
library(RColorBrewer)

# calculate mean, standard deviation, upper and lower quartiles of biomass enhancement for each species in each year

dat <- read.csv('outputs/biomass-enhancement.csv') #%>% filter(species != 'Australasian snapper')
dat_param <- read.csv('data/fish-dat.csv') %>% 
  select(species, harvested) %>% distinct() %>% 
  mutate(harvested = recode(harvested, 'y' = 'Harvested species', 'n' = 'Non-harvested species'))

# calculate net biomass enhancement per species at each site across all simulated values

dat2 <- dat %>% 
  left_join(select(dat_param, species, harvested), by = 'species') %>% 
  mutate(net_biomass_kg_ha = (net_biomass_g_unit_area/100)*10) %>% # unit area is 100m2, so divide by 100 to get g/m2, then multiply by 10 to get kg/ha
  filter(m == 'm') %>% # using selected 'm' values for main results
  group_by(sim, species, harvested, site, year) %>% 
  summarise(net_biomass_kg_ha = sum(net_biomass_kg_ha)) %>%  # here summing by gender 
  group_by(species, harvested, site, year) %>% # here summarizing across simulations for each site
  summarise(net_biomass_kg_ha_mean = mean(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_var = var(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_sd = sd(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_se = sd(net_biomass_kg_ha, na.rm = T)/sqrt(max(dat$sim)),
            net_biomass_kg_ha_95 = (sd(net_biomass_kg_ha, na.rm = T)/sqrt(max(dat$sim)))*1.96,
            net_biomass_kg_ha_upp = quantile(net_biomass_kg_ha, 0.75, na.rm = T),
            net_biomass_kg_ha_low = quantile(net_biomass_kg_ha, 0.25, na.rm = T))

# sum biomass at each site, then average across sites
# pool variance and weight by sample size (i.e., number of species at a site) to get SDs

site_average <- dat2 %>% 
  group_by(site, year) %>% # sum biomass and variance at each site across species
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_var = sum(net_biomass_kg_ha_var),
            n = n()) %>%
  group_by(year) %>% # average biomass enhancement across sites
  summarise(net_enhance = mean(net_enhance),
            net_sd = sqrt(weighted.mean(net_var, n)))
site_average = data.frame(site = 'Location average', site_average)

a <- dat2 %>% 
  group_by(site, year) %>% 
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_sd = sqrt(sum(net_biomass_kg_ha_var))) %>%
  rbind(site_average) %>% 
  mutate(site = factor(site, levels = c('Margarets Reef', 'Location average', 'Dromana', 'Glenelg'))) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_enhance - net_sd, 
                  ymax = net_enhance  + net_sd, group = site), fill = "grey", alpha = 0.2) +
  geom_line(aes(x = year, y = net_enhance, col = site)) +
  scale_color_manual(values = c("Margarets Reef" = "lightblue", 
                                "Location average" = 'black', 
                                "Dromana" = "pink",
                                'Glenelg' = 'lightgreen')) +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  ggtitle('A) Species summed by location, then averaged (+- SD)') +
  xlab('Year') +
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 11))
a

a2 <- dat2 %>% 
  group_by(harvested, site, year) %>% 
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
             net_var = sum(net_biomass_kg_ha_var),
             n = n()) %>%
  group_by(harvested, year) %>% 
  summarise(net_enhance = mean(net_enhance), 
            net_sd = sqrt(weighted.mean(net_var, n))) %>% 
  mutate(ymin = net_enhance - net_sd,
         ymax = net_enhance  + net_sd) %>% 
  mutate(ymin = ifelse(ymin <0, 0, ymin), 
         ymax = ifelse(ymax < 0, 0, ymax)) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = ymin, 
                  ymax = ymax, group = harvested), fill = "grey", alpha = 0.2) +
  geom_line(aes(x = year, y = net_enhance, col = harvested)) +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  ggtitle('B) All species (+- SD): Harvested vs. non-harvested') +
  xlab('Year') +
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 11))
a2 

# species with uncertainty

b <- dat2 %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_biomass_kg_ha_mean - net_biomass_kg_ha_sd, 
                  ymax = net_biomass_kg_ha_mean + net_biomass_kg_ha_sd, group = species), fill = "grey", alpha = 0.2) +
  geom_line(aes(x = year, y = net_biomass_kg_ha_mean, col = species)) +
  ylab('') +
  xlab('Year') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~year^-1*')')) +
  ggtitle('C) Individual species (+- SD) at each site') +
  facet_wrap(~site, scales = 'free_y') +
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 11))
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
  theme_classic() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 11))
c

# plot together

d <- a+a2 # plot_layout(nrow = 1, widths = c(0.5, 1))
g <- d/b/c
g

ggsave('outputs/bioenhancement.png', width = 10.5, height = 10.5)

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

