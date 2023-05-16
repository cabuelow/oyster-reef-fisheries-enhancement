# summarise results and plot

library(tidyverse)
library(patchwork)
library(RColorBrewer)

# calculate mean, standard deviation, upper and lower quartiles of biomass enhancement for each species in each year

dat <- read.csv('outputs/biomass-enhancement.csv') 
dat[is.na(dat)] <- 0 # make all missing biomass enhancement values 0 (its just because the fish hasn't recruited yet)

dat2 <- dat %>% 
  filter(m == 'm') %>% # using lit-derived mortality rate
  group_by(sim, species, year) %>% 
  summarise(cumul_bio_enhance_g_ha = sum(cumul_bio_enhance_g_ha)) %>%  # here summing by gender 
  group_by(species, year) %>% 
  summarise(cumul_benhance_mean = mean(cumul_bio_enhance_g_ha, na.rm = T),
            cumul_benhance_var = var(cumul_bio_enhance_g_ha, na.rm = T),
            cumul_benhance_sd = sd(cumul_bio_enhance_g_ha, na.rm = T),
            cumul_benhance_se = sd(cumul_bio_enhance_g_ha, na.rm = T)/sqrt(max(dat$sim)),
            cumul_benhance_95 = (sd(cumul_bio_enhance_g_ha, na.rm = T)/sqrt(max(dat$sim)))*1.96,
            cumul_benhance_upp = quantile(cumul_bio_enhance_g_ha, 0.75, na.rm = T),
            cumul_benhance_low = quantile(cumul_bio_enhance_g_ha, 0.25, na.rm = T))

# total biomass enhancement across all species
# have same sample size for each species in each year, so don't need to weight variance or biomass esimtates by sample size

a <- dat2 %>% 
  group_by(year) %>% 
  summarise(total_behnance = sum(cumul_benhance_mean), 
            total_sd = sqrt(sum(cumul_benhance_var))) %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = total_behnance - total_sd, 
                  ymax = total_behnance + total_sd), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = total_behnance)) +
  ylab(bquote('Biomass enhancement (g ' ~ha^-1~year^-1*')')) +
  ggtitle('A) All species (+- SD)') +
  xlab('Year') +
  ylim(c(0,450)) +
  theme_classic()

# species with uncertainty

b <- dat2 %>% 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = cumul_benhance_mean - cumul_benhance_sd, 
                  ymax = cumul_benhance_mean + cumul_benhance_sd, group = species), fill = "grey", alpha = 0.5) +
  geom_line(aes(x = year, y = cumul_benhance_mean, col = species)) +
  ylab('') +
  xlab('Year') +
  ggtitle('B) Individual species (+- SD)') +
  ylim(c(0,450)) +
  theme_classic() +
  theme(legend.position = 'none')

# plot by species

c <- dat2 %>% 
  ggplot() +
  geom_area(aes(x = year, y = cumul_benhance_mean, fill = factor(species)), position = 'stack') +
  ylab('') +
  xlab('Year') +
  ggtitle('C) Cumulative individual species') +
  ylim(c(0,450)) +
  theme_classic() +
  theme(legend.title = element_blank()) 

# plot together

g <- a+b+c
g

ggsave('outputs/bioenhancement.png', width = 12, height = 4)

# plot sensitivity to m, total and species

e <- dat %>% 
  filter(year == 25) %>% # pick max year where all species enhancements will be stable
  group_by(m, sim, species) %>% 
  summarise(cumul_bio_enhance_g_ha = sum(cumul_bio_enhance_g_ha)) %>% # sum by gender
  ggplot() +
  aes(x = m, y = cumul_bio_enhance_g_ha) +
  geom_jitter(alpha = 0.1, size = 0.2, width = 0.1) +
  geom_violin(fill = 'transparent', col = 'cyan') +
  ggtitle('A) All species') +
  ylab(bquote('Biomass enhancement (g ' ~ha^-1~year^-1*')')) +
  xlab('') +
  theme_classic()
e  

f <- dat %>% 
  filter(year == 25) %>% # pick max year where all species enhancements will be stable
  group_by(m, sim, species) %>% 
  summarise(cumul_bio_enhance_g_ha = sum(cumul_bio_enhance_g_ha)) %>% # sum by gender
  ggplot() +
  aes(x = m, y = cumul_bio_enhance_g_ha) +
  geom_jitter(alpha = 0.1, size = 0.2, width = 0.1) +
  geom_violin(fill = 'transparent', col = 'cyan') +
  facet_grid(~species) +
  ggtitle('B) Individual species') +
  ylab('') +
  xlab('')+
  theme_classic()
f  

h <- e+f+plot_layout(widths = c(0.2,1))
h

ggsave('outputs/bioenhancement_mortality-sensitivity.png', width = 12, height = 4)

# plot everything

g/h

ggsave('outputs/bioenhancement_plots_all.png', width = 12, height = 8)

