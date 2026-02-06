### Species richness & biodiversity ###

## Calculate species richness & Shannon's index for soils investigated in class
## Plot richness & diversity

library(here)
library(tidyverse)
#library(googlesheets4)

####################
### Read in data ###
####################
#dat <- read_sheet("https://drive.google.com/drive/folders/1bFR6wPmL9ZUbDHyRIZ-npGIuhi29kgXY", sheet = "Sheet1")

#write.csv(dat, here("data", "teabags_global.csv"), row.names = FALSE)

dat <- read.csv(here("data", "teabags_global.csv"))

## Recode attrbiutes
dat <- dat %>% mutate(location_type = ifelse(location_type == 0, "undefined",
                                          ifelse(location_type == 1, "ambient",
                                              ifelse(location_type == 2, "agriculture", "other treatment")))) %>%
              filter(id < 4227) %>%
              mutate(biome = comment)

####################
### Explore data ###
####################

# display first 5 rows of data
head(dat)

# View entire data file in data viewer tab 
View(dat)

# Display column names
names(dat)

####################
### Data summary ###
####################
## Mean decomposition rates by biome
dat.mn <- dat %>% group_by(biome) %>%
                  summarize(n = n(),
                            mean = mean(k, na.rm = TRUE),
                            sd = sd(k, na.rm = TRUE))

#############
### Plots ###
#############
# basic plot
biome.pl <- dat %>% filter(biome != "" & biome != "#N/A") %>%
                    ggplot(aes(x = biome, y = k)) +
                      geom_point() +
                      geom_boxplot(outlier.shape = NA)
                      
# Now let's clean up the aesthetics
biome.pl2 <- dat %>% filter(biome != "" & biome != "#N/A") %>%
                     ggplot(aes(x = biome, y = k)) +
                        geom_point(aes(color = biome), position = position_jitter()) +
                        geom_boxplot(aes(fill = biome), outlier.shape = NA, alpha = 0.3) +
                        ylab(expression("decomposition rate, k ("*d^-1*")")) +
                        scale_y_log10() +
                        theme_bw() +
                        theme(legend.position = "none",
                              panel.grid = element_blank(),
                              panel.border = element_rect(color = "black", linewidth = 2),
                              axis.text = element_text(size = 20),
                              axis.text.x = element_text(angle = 45, hjust = 1),
                              axis.title = element_text(size = 20, margin = margin(t = 0, r = 11, b = 0, l = 0)))

## What does fill do?
## What does color do?
## What does alpha do?

ggsave(biome.pl2, path = "plots", file = "tea_k_biome.pdf", width = 8, height = 10, units = "in")

# Visualize patterns by latitude
lat.pl <- dat %>% filter(biome != "" & biome != "#N/A") %>%
                  ggplot(aes(x = abs(latitude), y = k)) +
                    geom_point(aes(color = biome)) +
                    ylab(expression("decomposition rate, k ("*d^-1*")")) +
                    xlab("latitude (degrees N or S)") +
                    scale_y_log10() +
                  theme_bw() +
                  theme(legend.position = "right",
                        legend.text = element_text(size = 12),
                        legend.title = element_text(size = 12),
                        panel.grid = element_blank(),
                        panel.border = element_rect(color = "black", linewidth = 2),
                        axis.text = element_text(size = 20),
                        axis.text.x = element_text(angle = 45, hjust = 1),
                        axis.title = element_text(size = 20, margin = margin(t = 0, r = 11, b = 0, l = 0)))

ggsave(lat.pl, file = "tea_k_lat.pdf", path = "plots", width = 10, height = 7, units = "in")

# 1) Plot data by land use type (location_type)

# 2) Add land use type to the latitude plot

# 3) Does estimated decomposition rate depend on the duration of incubation?

############################
### Richness & diversity ###
############################
## Reshape to long format
datL <- dat %>% pivot_longer(cols = orange_beetle:Isopod, names_to = "Organism", values_to = "Count")

rich <- datL %>% group_by(SoilType, Replicate) %>%
                 filter(!is.na(Count)) %>%
                 distinct(Organism) %>%
                 summarize(richness = n())

evenness <- datL %>% group_by(SoilType, Replicate) %>%
                     filter(!is.na(Count)) %>%
                     reframe(evenn = Count/sum(Count)) 

div <- evenness %>% group_by(SoilType, Replicate) %>%
                    reframe(H = -sum(evenn*log(evenn)))

write.csv(div, here("data", "ShannonH.csv"), row.names = FALSE)

#############
### Plots ###
#############
# basic plot
rich.pl <- rich %>% ggplot(aes(x = SoilType, y = richness, groups = Replicate)) +
                      geom_bar(aes(color = Replicate), position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity")

## What does fill do?
## What does color do?
## What does position do?

# Now let's clean up the aesthetics
rich.pl2 <- rich %>% ggplot(aes(x = SoilType, y = richness, groups = Replicate)) +
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
                            axis.text.x = element_text(angle = 45, hjust = 1),
                            axis.title = element_text(size = 20, margin = margin(t = 0, r = 11, b = 0, l = 0)))
                            
ggsave(rich.pl2, path = "plots", file = "spprichness.pdf", width = 12.5, height = 9.5, units = "in")

## Plot evenness

## Plot Shannon's H


