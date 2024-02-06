# ordination of species presence-absence on oyster reefs

library(tidyverse)
library(vegan)
library(ggrepel)
library(patchwork)
set.seed(22) # set random number generator

# read in data read and perpare data for multivariate analyses

dat <- read.csv('data/juvenile_densities_multivariate_20240131.csv') %>% 
  mutate(Site = ifelse(Site == 'Margarets Reef', 'Margaret', Site)) %>% 
  filter(habitat != 'Seagrass') %>% 
  mutate(habitat = ifelse(habitat %in% c('Edge', 'Flat'), 'Reef', habitat)) %>% 
  select(Site, habitat, class.name, deployment.code, density) %>% 
  pivot_wider(names_from = 'class.name', values_from = 'density') %>% 
  mutate(across(`Chrysophrys auratus`:`Aracana ornata`, ~ifelse(is.na(.), 0, .))) %>% 
  select(where(~ any(. != 0))) %>% # remove spp with no observations (seven)
  mutate(across(`Chrysophrys auratus`:`Aracana ornata`, ~ifelse(.>0, 1, .))) %>% # presence-absence
  select(-c(`Trygonorrhina dumerilii`, `Arripis trutta`, `Sphyraena novaehollandiae`, `Hyporhamphus melanochir`, `Ovalipes australiensis`, `Trachurus novaezelandiae`,`Meuschenia scaber`, `Ophthalmolepis lineolatus`, `Trachinops caudimaculatus`)) %>%  # remove spp occuring in less than 5% of sites
  rowwise() %>%
  mutate(total = sum(c_across(`Chrysophrys auratus`:`Aracana ornata`))) %>% 
  filter(total > 0) %>% # remove rows without any observations
  select(-total) %>% 
  mutate(Site = as.factor(Site))

# run nmds

nmds <- metaMDS(dat[,-c(1:3)], k = 2, autotransform = F, trymax = 100, distance = 'jaccard')
nmds
stressplot(nmds)

# get nmds scores to plot

nmds_scores <- data.frame(scores(nmds)$sites)
nmds_scores$Location <- dat$Site
nmds_scores$Habitat <- dat$habitat
nmds_scores$Location_Habitat <- paste0(dat$Site, '_', dat$habitat)

# get species scores to plot

nmds_species <- data.frame(scores(nmds)$species)
nmds_species$Label <- row.names(nmds_species)

# plot

a <- ggplot() +
  #geom_point(data = nmds_scores, aes(x = NMDS1, y = NMDS2, col = Location_Habitat, #shape = Habitat, 
   #                                   alpha = 0.9), size = 2) +
  geom_jitter(data = nmds_scores, aes(x = NMDS1, y = NMDS2, col = Location, shape = Habitat, 
                                     alpha = 0.9), size = 2, width = 0.5) +
  stat_ellipse(data = nmds_scores, aes(x = NMDS1, y = NMDS2, col = Location, alpha = 0.9), level = 0.95) +
  geom_point(data = nmds_species, aes(x = NMDS1, y = NMDS2), size = 0.2) +
  geom_segment(data = nmds_species, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), linewidth = 0.5, alpha = 0.5, colour = "grey30") +
  geom_text_repel(data = nmds_species, aes(x = NMDS1, y = NMDS2, label = Label), size = 2, alpha = 0.7) +
  scale_color_brewer(palette = 'Set2') +
  xlab('NMDS1') +
  ylab('NMDS2') +
  theme_classic() +
  guides(alpha = 'none')
a

ggsave('outputs/ordination/nmds.png', width = 5.5, height = 3.5)

# permanova - permutational multivariate analysis of variance
# site and habitat are crossed factors (as opposed to nested) and we want to test for an interaction as well as main effects
# note that ordering of terms is important, as terms are added sequentially (first to last) using type 2 sums of squares
# so which term goes into the model first determines how much variation is left to be explained by the next covariate
# alternatively can use test for the marginal effects of each main and interactive effect after controlling for the effects

# first check for multivariate homogeneity of group dispersions (variances) (multivariate analogue of Levene's test for homogeneity of variance)
anova(betadisper(vegdist(dat[,-c(1:3)], 'jaccard'), dat$Site))
anova(betadisper(vegdist(dat[,-c(1:3)], 'jaccard'), factor(paste0(dat$habitat,'_', dat$Site))))
# have unequal variance so could confound our analysis of variance testing for differences in spp. compositions (PERMANOVA)

# run permanova
# sequential terms, try different orders
adonis2(dat[,-c(1:3)] ~ Site*habitat, data = dat[,c(1:3)], method = 'jaccard')
adonis2(dat[,-c(1:3)] ~ habitat*Site, data = dat[,c(1:3)], method = 'jaccard')

# diversity indices 

dat <- read.csv('data/juvenile_densities_multivariate_20240131.csv') %>% 
  filter(habitat != 'Seagrass') %>% 
  mutate(habitat = ifelse(habitat %in% c('Edge', 'Flat'), 'Reef', habitat)) %>% 
  dplyr::group_by(Site, habitat, class.name) %>% 
  dplyr::summarise(density = mean(density)) %>% 
  pivot_wider(names_from = 'class.name', values_from = 'density') %>% 
  mutate(across(`Acanthaluteres spilomelanurus`:`Upeneichthys vlamingii`, ~ifelse(is.na(.), 0, .))) %>% 
  select(where(~ any(. != 0))) # remove spp with no observations (seven)

Richness <- apply(dat[,-c(1:2)]>0, 1, sum)
div <- data.frame(dat[,c(1:2)], 
                  Richness = Richness,
                  Shannon_index = diversity(dat[,-c(1:2)], index = 'shannon'),
                  Simpson_index = diversity(dat[,-c(1:2)], index = 'simpson'),
                  InvSimpson_index = diversity(dat[,-c(1:2)], index = 'invsimpson'),
                  Evenness_index = diversity(dat[,-c(1:2)], index = 'simpson')/log(Richness))

write.csv(div, 'outputs/ordination/diversity-metrics.csv', row.names = F)
