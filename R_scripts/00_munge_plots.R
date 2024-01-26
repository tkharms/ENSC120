### Species richness & biodiversity ###

## Calculate species richness & Shannon's index for soils investigated in class
## Plot richness & diversity

library(here)
library(tidyverse)

####################
### Read in data ###
####################
dat <- read.csv(here("data", "ensc120_spprich.csv"))

####################
### Explore data ###
####################

# display first 5 rows of data
head(dat)

# View entire data file in data viewer tab 
View(dat)

# Display column names
names(dat)

############################
### Richness & diversity ###
############################
rich <- dat %>% group_by(SoilType, Replicate) %>%
                distinct(Organism) %>%
                summarize(richness = n())

evenness <- dat %>% group_by(SoilType, Replicate) %>%
                    reframe(evenn = Count/sum(Count)) 

div <- evenness %>% group_by(SoilType, Replicate) %>%
                    reframe(H = -sum(evenn*log(evenn)))

write.csv(div, here("data", "ShannonH.csv"), row.names = FALSE)

#############
### Plots ###
#############
# basic plot
rich.pl <- rich %>% ggplot(aes(x = SoilType, y = richness, groups = Replicate)) +
                      geom_bar(aes(color = Replicate, fill = Replicate), position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity")
                      
## What does fill do?
## What does color do?
## What does position do?

# Now let's clean up the aesthetics
rich.pl <- rich %>% ggplot(aes(x = SoilType, y = richness, groups = Replicate)) +
                      geom_bar(aes(color = Replicate, fill = Replicate), position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity") +
                      ylab("# species") +
                      theme_bw() +
                      theme(legend.position = c(0.85, 0.8),
                            legend.background = element_rect(fill = NA, color = NA),
                            legend.text = element_text(size = 20),
                            legend.title = element_text(size = 20),
                            panel.grid = element_blank(),
                            panel.border = element_rect(color = "black", linewidth = 2),
                            axis.text = element_text(size = 20),
                            axis.title = element_text(size = 20, margin = margin(t = 0, r = 11, b = 0, l = 0)))
                            
ggsave(rich.pl, path = "plots", file = "spprichness.pdf", width = 12.5, height = 9.5, units = "in")

## Plot evenness

## Plot Shannon's H


