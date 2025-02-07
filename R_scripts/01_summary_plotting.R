### Summarizing & visualizing data ###
## Global decomposition dataset
# Bradford, Mark A. et al. (2018). Data from: A test of the hierarchical model of litter decomposition [Dataset]. Dryad. https://doi.org/10.5061/dryad.c44h0

library(here)
library(tidyverse)
library(gtable)
library(gridExtra)
library(grid)

####################
### Read in data ###
####################
dat <- read.csv(here("data", "BradfordVeenLitDecompDATA.csv"))

####################
### Data summary ###
####################
### Mean & standard deviation by site and litter type
sdat <- dat %>% group_by(Site, Lit, Tea) %>%
                summarize(across(Temp1:LitFinCN, list(n = ~sum(!is.na(.)),
                                                      mean = ~mean(., na.rm = TRUE),
                                                      sd = ~sd(., na.rm = TRUE))))

#############
### Plots ###
#############
### Mean decomp rate w/ SD
teamn.pl <- sdat %>% ggplot(aes(x = Site, y = STeaLoss_mean, groups = Tea)) +
                     geom_pointrange(aes(ymin = STeaLoss_mean - STeaLoss_sd, ymax = STeaLoss_mean + STeaLoss_sd, color = Tea), position = position_jitter(width = 0.15), fatten = 10, size = 0.15) +
                     ylab("% mass loss in 90 d") +
                     scale_color_manual("tea type", values=c("darkgreen", "darkred")) +
                     theme_bw() +
                     ylim(0, 75) +
                     theme(axis.title.x = element_blank(),
                        axis.text = element_text(size = 18),
                        #axis.title.y = element_text(size = 20),
                        panel.grid = element_blank(),
                        legend.title = element_text(size = 16),
                        legend.title.align = 0.5,
                        legend.text = element_text(size = 16),
                        legend.position = c(0.1, 0.925)
                  )

litmn.pl <- sdat %>% ggplot(aes(x = Site, y = SLitLoss_mean, groups = Lit)) +
                     geom_pointrange(aes(ymin = SLitLoss_mean - SLitLoss_sd, ymax = SLitLoss_mean + SLitLoss_sd, color = Lit), position = position_jitter(width = 0.15), fatten = 10, size = 0.5) +
                     ylab("% mass loss in 90 d") +
                     ylim(0,75) +
                     scale_color_manual("litter type", values=c("darkblue", "darkgoldenrod")) +
                     theme_bw() +
                     theme(axis.title.x = element_blank(),
                        axis.text = element_text(size = 18),
                        axis.title.y = element_text(size = 20),
                        panel.grid = element_blank(),
                        legend.title = element_text(size = 16),
                        legend.title.align = 0.5,
                        legend.text = element_text(size = 16),
                        legend.position = c(0.1, 0.925)
  )
  
# Save multiple panels
ga <- ggplotGrob(teamn.pl)
gb <- ggplotGrob(litmn.pl)

grid.arrange(ga, gb, nrow = 1)

decomp.pl <- arrangeGrob(ga, gb, nrow = 1)

ggsave(path = "plots", file = "tea_litter_massloss.pdf", decomp.pl, width = 16, height = 8, units ="in")

### Scatterplot matrix
pairs(~SLitLoss_mean + 
        pH_mean + 
        SOM_mean +
        Moist1_mean +
        percNsoil_mean, 
        data = sdat, col = as.factor(sdat$Lit))

### Single scatterplot
LitN.pl <- dat %>% ggplot(aes(x = Temp1, y = SLitLoss)) +
                   geom_point(aes(color = Site)) +
                   facet_grid(rows = vars(Lit), cols = vars(Site), scales = "free_x")
  
#################
### Exercises ###
#################
#1) Determine the effect of three of the commands in the pointrange plot (mass loss by site).

#2) How do environmental factors influence mass loss of tea across the sites? Use a figure to support your reasoning.

#3) How do environmental factors influence mass loss of tea within each site? How do environmental effects within each site compare to environmental effects across sites?