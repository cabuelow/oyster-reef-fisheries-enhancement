# summarise results and plot

library(tidyverse)
library(patchwork)
library(ggh4x)
library(RColorBrewer)

dat <- read.csv('outputs/biomass-enhancement.csv')
dat_param <- read.csv('data/fish-dat.csv') %>% 
  select(species, harvested) %>% distinct() %>% 
  mutate(harvested = recode(harvested, 'y' = 'Harvested species', 'n' = 'Non-harvested species'))

# calculate net biomass enhancement and uncertainty per species at each site across all simulated values

dat2 <- dat %>% 
  left_join(select(dat_param, species, harvested), by = 'species') %>% 
  mutate(net_biomass_kg_ha = (net_biomass_g_unit_area/100)*10) %>% # unit area is 100m2, so divide by 100 to get g/m2, then multiply by 10 to get kg/ha
  group_by(m, sim, species, harvested, site, year) %>% 
  summarise(net_biomass_kg_ha = sum(net_biomass_kg_ha)) %>%  # here summing by gender 
  group_by(m, species, harvested, site, year) %>% # here summarizing across simulations for each site
  summarise(net_biomass_kg_ha_mean = mean(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_var = var(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_sd = sd(net_biomass_kg_ha, na.rm = T),
            net_biomass_kg_ha_se = sd(net_biomass_kg_ha, na.rm = T)/sqrt(max(dat$sim)),
            net_biomass_kg_ha_95 = (sd(net_biomass_kg_ha, na.rm = T)/sqrt(max(dat$sim)))*1.96,
            net_biomass_kg_ha_upp = quantile(net_biomass_kg_ha, 0.75, na.rm = T),
            net_biomass_kg_ha_low = quantile(net_biomass_kg_ha, 0.25, na.rm = T))

# save each species biomass enhancement at equilibirum

write.csv(filter(dat2, year == max(dat$year) & m == 'm_final'), 'outputs/net-bio-enhancement_equilibrium.csv', row.names = F)

# sum biomass at each site, then average across sites

site_average <- dat2 %>% 
  filter(m == 'm_final') %>% # using selected 'm' values for main results
  group_by(site, year) %>% # sum biomass and variance at each site across species
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_var = sum(net_biomass_kg_ha_var),
            n = n()) %>%
  group_by(year) %>% # average biomass enhancement across sites
  #summarise(net_enhance = weighted.mean(net_enhance, n),
   #         net_sd = sqrt(weighted.mean(net_var, n)))
  summarise(net_enhance = mean(net_enhance),
            net_sd = sqrt(mean(net_var)))
site_average <- data.frame(site = 'Average all locations', site_average)

a <- dat2 %>% 
  filter(m == 'm_final') %>% # using selected 'm' values for main results
  group_by(site, year) %>% 
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_sd = sqrt(sum(net_biomass_kg_ha_var))) %>%
  rbind(site_average) %>% 
  mutate(site = factor(site, levels = c('Margarets Reef', 'Average all locations', 'Dromana', 'Glenelg'))) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_enhance - net_sd, 
                  ymax = net_enhance  + net_sd, group = site), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = year, y = net_enhance, col = site, linewidth = site), alpha = 0.7) +
  scale_color_manual(values = c("Margarets Reef" = "cyan3", 
                                "Average all locations" = 'black', 
                                "Dromana" = "palevioletred1",
                                'Glenelg' = 'seagreen2'),
                     labels = c("Margaret", 'Average all locations', 'Dromana', 'Glenelg')) +
  scale_linewidth_manual(values = c(1, 2, 1, 1), guide = 'none') +
  scale_y_continuous(labels = scales::comma) +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  ggtitle('A) Species summed by location, then averaged') +
  xlab('Year') +
  theme_classic() +
  theme(legend.title = element_blank(),
        #legend.position = 'bottom',
        plot.title = element_text(size = 11)) #+
  #guides(color=guide_legend(nrow=2,byrow=TRUE))
a

a2 <- dat2 %>% 
  filter(m == 'm_final') %>% # using selected 'm' values for main results
  group_by(harvested, site, year) %>% # total biomass enhancement at each site (harvested vs non-harvested)
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
             net_var = sum(net_biomass_kg_ha_var),
             n = n()) %>%
  group_by(harvested, year) %>% # average across locations
  #summarise(net_enhance = weighted.mean(net_enhance, n), 
   #         net_sd = sqrt(weighted.mean(net_var, n))) %>% 
  summarise(net_enhance = mean(net_enhance),
            net_sd = sqrt(mean(net_var))) %>% 
  mutate(ymin = net_enhance - net_sd,
         ymax = net_enhance  + net_sd) %>% 
  mutate(ymin = ifelse(ymin <0, 0, ymin), 
         ymax = ifelse(ymax < 0, 0, ymax)) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = ymin, 
                  ymax = ymax, group = harvested), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = year, y = net_enhance, col = harvested), linewidth = 1) +
  scale_color_manual(values = c("Harvested species" = "indianred2", 
                                "Non-harvested species" = 'darkslategray4')) +
  scale_y_continuous(labels = scales::comma) +
  #ylab('') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  ggtitle('B) Harvested vs. Non-Harvested species') +
  xlab('Year') +
  theme_classic() +
  theme(legend.title = element_blank(),
        #legend.position = 'bottom',
        plot.title = element_text(size = 11)) #+
  #guides(color=guide_legend(nrow=2,byrow=TRUE))
a2 

a + a2

ggsave('outputs/bioenhancement_Fig2.png', width = 10, height = 3)

# snapper vs. not snapper

b <- dat2 %>% 
  filter(m == 'm_final' & site != 'Glenelg') %>% # using selected 'm' values for main results
  mutate(snap = ifelse(species == 'Australasian snapper', 'Australasian snapper', 'Other species')) %>% 
  mutate(site = ifelse(site == 'Margarets Reef', 'Margaret', site)) %>% 
  group_by(snap, site, year) %>% 
  summarise(net_enhance = sum(net_biomass_kg_ha_mean), 
            net_sd = sqrt(sum(net_biomass_kg_ha_var))) %>%
  mutate(site = factor(site, levels = c('Margaret', 'Average all locations', 'Dromana', 'Glenelg'))) %>% 
  mutate(ymin = net_enhance - net_sd,
         ymax = net_enhance  + net_sd) %>% 
  mutate(ymin = ifelse(ymin <0, 0, ymin), 
         ymax = ifelse(ymax < 0, 0, ymax)) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = ymin, 
                  ymax = ymax, group = snap), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = year, y = net_enhance, col = snap), linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~site) + #scales = 'free_y') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  ggtitle('A) Australasian snapper vs. all other species') +
  xlab('Year') +
  theme_classic() +
  theme(legend.title = element_blank(),
        #legend.position = 'bottom',
        plot.title = element_text(size = 11)) #+
  #guides(color=guide_legend(nrow=1,byrow=TRUE))
b

ggsave('outputs/bioenhancement_Fig3A.png', width = 7, height = 3)

# top 3 species with uncertainty

drom <- dat2 %>%
  filter(m == 'm_final') %>% # using selected 'm' values for main results
  filter(year == 40 & site == 'Dromana') %>% 
  arrange(desc(net_biomass_kg_ha_mean))

marg <- dat2 %>%
  filter(m == 'm_final') %>% # using selected 'm' values for main results
  filter(year == 40 & site == 'Margarets Reef') %>% 
  arrange(desc(net_biomass_kg_ha_mean))

glen <- dat2 %>%
  filter(m == 'm_final') %>% # using selected 'm' values for main results
  filter(year == 40 & site == 'Glenelg') %>% 
  arrange(desc(net_biomass_kg_ha_mean))

dromana <- dat2 %>% filter(m == 'm_final') %>% filter(site == 'Dromana' & species %in% drom$species[2:4])
margaret <- dat2 %>% filter(m == 'm_final') %>% filter(site == 'Margarets Reef' & species %in% marg$species[2:4])
glenelg <- dat2 %>% filter(m == 'm_final') %>% filter(site == 'Glenelg' & species %in% glen$species[1:3])

b2 <- margaret %>% 
  mutate(site = ifelse(site == 'Margarets Reef', 'Margaret', site)) %>% 
  mutate(species = factor(species, levels = marg$species[2:4])) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_biomass_kg_ha_mean - net_biomass_kg_ha_sd, 
                  ymax = net_biomass_kg_ha_mean + net_biomass_kg_ha_sd, group = species), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = year, y = net_biomass_kg_ha_mean, col = species), linewidth = 1) +
  scale_color_manual(values = c("seagreen3", 'goldenrod2', 'darkslategray4')) +
  ylab('') +
  xlab('Year') +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  ggtitle('B) Top 3 species by location') +
  facet_wrap(~site, scales = 'free_y') +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size = 11)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE))
b2

b3 <- dromana %>% 
  group_by(site, species, year) %>% 
  mutate(species = factor(species, levels = drom$species[2:4])) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_biomass_kg_ha_mean - net_biomass_kg_ha_sd, 
                  ymax = net_biomass_kg_ha_mean + net_biomass_kg_ha_sd, group = species), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = year, y = net_biomass_kg_ha_mean, col = species), linewidth = 1) +
  scale_color_manual(values = c("seagreen3", 'goldenrod2', 'darkslategray4')) +
  ylab('') +
  xlab('Year') +
  #ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  #ggtitle('C) Individual species at each location') +
  facet_wrap(~site, scales = 'free_y') +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size = 11)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE))
b3

b4 <- glenelg %>% 
  group_by(site, species, year) %>% 
  mutate(species = factor(species, levels = glen$species[1:3])) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = net_biomass_kg_ha_mean - net_biomass_kg_ha_sd, 
                  ymax = net_biomass_kg_ha_mean + net_biomass_kg_ha_sd, group = species), fill = "grey", alpha = 0.3) +
  geom_line(aes(x = year, y = net_biomass_kg_ha_mean, col = species), linewidth = 1) +
  scale_color_manual(values = c("seagreen3", 'goldenrod2', 'darkslategray4')) +
  ylab('') +
  xlab('Year') +
  #ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  #ggtitle('C) Individual species at each location') +
  facet_wrap(~site, scales = 'free_y') +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        plot.title = element_text(size = 11)) +
  guides(color=guide_legend(nrow=3,byrow=TRUE))
b4

b2+b3+b4

ggsave('outputs/bioenhancement_Fig3B.png', width = 8.5, height = 4.1)

# combine into one fig

#b/b2+b3+b4+plot_layout(design = c(area(1,1,1,2), area(2,1,2,1), area(2,2,2,2), area(2,3,2,3)))
#ggsave('outputs/bioenhancement_Fig3AB.png', width = 8, height = 7)

# plot sensitivity to m, total and species

spp_remove <- read.csv('data/fish-dat.csv') %>% filter(is.na(m)) %>% pull(species) %>% unique()

dat3 <- dat %>% 
  filter(!species %in% spp_remove) %>% # remove species that don't have a literature derived M value 
  filter(year == max(dat$year) & m != 'm_final' & site != 'Glenelg' & species %in% c(marg$species[1:5], drom$species[1:5])) %>% # pick max year where all species enhancement will be stable
  #mutate(snap = ifelse(species == 'Australasian snapper', 'Australasian snapper', 'Other species')) %>% 
  mutate(site = factor(ifelse(site == 'Margarets Reef', 'Margaret', site), levels = c('Margaret', 'Dromana'))) %>% 
  mutate(m = ifelse(m == 'm', 'M derived from literature', 'M derived from max age')) %>% 
  mutate(net_biomass_kg_ha = (net_biomass_g_unit_area/100)*10) %>% # unit area is 100m2, so divide by 100 to get g/m2, then multiply by 10 to get kg/ha
  group_by(m, sim, species, site) %>% 
  summarise(net_biomass_kg_ha = sum(net_biomass_kg_ha)) # here summing by gender 

e <- dat3 %>% 
  filter(site == 'Margaret') %>% 
  ggplot() +
  aes(x = m, y = net_biomass_kg_ha) +
  geom_jitter(alpha = 0.1, size = 0.2, width = 0.1) +
  geom_violin(fill = 'transparent', col = 'cyan3', linewidth = 1) +
  facet_nested(~site + species, scales = 'free_y', independent = 'y') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  xlab('') +
  theme_classic()
e  

f <- dat3 %>% 
  filter(site == 'Dromana') %>% 
  ggplot() +
  aes(x = m, y = net_biomass_kg_ha) +
  geom_jitter(alpha = 0.1, size = 0.2, width = 0.1) +
  geom_violin(fill = 'transparent', col = 'cyan3', linewidth = 1) +
  facet_nested(~site + species, scales = 'free_y', independent = 'y') +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylab(bquote('Biomass enhancement (kg ' ~ha^-1~yr^-1*')')) +
  xlab('') +
  theme_classic()
f  

g <- e/f #+ plot_layout(design = c(area(t = 1, l = 1, b = 1, r = 6),
          #                        area(t = 2, l = 1, b = 2, r = 5)))
g

ggsave('outputs/mortality-sensitivity.png', width = 11, height = 6)


