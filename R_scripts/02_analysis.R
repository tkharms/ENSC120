### Basic statistical analysis ###
## Global decomposition dataset
# Bradford, Mark A. et al. (2018). Data from: A test of the hierarchical model of litter decomposition [Dataset]. Dryad. https://doi.org/10.5061/dryad.c44h0

library(here)
library(tidyverse)
library(coin)

####################
### Read in data ###
####################
### Decomposition data
dat <- read.csv(here("data", "BradfordVeenLitDecompDATA.csv"))

##########################
### Data visualization ###
##########################
dat %>% ggplot(aes(x = Site, y = STeaLoss, fill = Tea)) +
          geom_boxplot(outlier.shape = NA) +
          geom_point(position = position_jitterdodge())
          

####################
### Data summary ###
####################
### Mean & standard deviation by site and litter type
sdat <- dat %>% group_by(Site, Lit, Tea) %>%
  summarize(across(Temp1:LitFinCN, list(n = ~sum(!is.na(.)),
                                        mean = ~mean(., na.rm = TRUE),
                                        sd = ~sd(., na.rm = TRUE))))

## Visualize summarized data
sdat %>% ggplot(aes(x = Tea, y = STeaLoss_mean)) +
  geom_point(aes(color = Site), size = 3) +
  geom_line(aes(group = Site, color = Site))

#########################
### Statistical tests ###
#########################
### t-test: Compare two means

## In the Bradford dataset, the litter and tea types are all incubated at each site, so the observations are paired. We will account for that non-independence using a paired t-test.
##
# 1) Move paired observations of decomp rate for red and green tea into two columns
# 2) Calculate difference in decomposition rate for red & green tea in each quadrat
# 3) Site-level means. We could make use of the replicates within sites, but that would require a more complex model. We'll keep it simple here.

ptdat <- dat %>% select(c(Site, Quadrat, Tea, STeaLoss)) %>%
                 pivot_wider(names_from = Tea, values_from = STeaLoss) %>%
                 mutate(diffs = Red - Green) %>%
                 group_by(Site) %>%
                 summarize(across(Green:diffs, list(n = ~sum(!is.na(.)),
                                                  mean = ~mean(., na.rm = TRUE),
                                                  sd = ~sd(., na.rm = TRUE))))

## Check assumptions of paired t-test
# Normality of differences using normal probability plot
qqnorm(ptdat$diffs_mean)
qqline(ptdat$diffs_mean)
# (Difficult to assess with only 6 observations)

t.test(ptdat$Red_mean, ptdat$Green_mean, paired = TRUE, alternative = "two.sided")
  
## Alternatively, use a resampling (bootstrap) approach
#resampling test for paired differences from coin package
coin::wilcoxsign_test(ptdat$Red_mean ~ ptdat$Green_mean, distribution="exact")

### ANOVA: Compare means across two or more groups ###

# Note that the model below is too simple for the paired design of the Bradford dataset. Code is provided for example purposes only.

mod1 <- lm(STeaLoss ~ Site*Tea, data = dat)
 
## Check assumptions on residuals 
# Normality of residuals using normal probability plot
qqnorm(resid(mod1))
qqline(resid(mod1))

# Homogeneity of variances
plot(mod1)

## Results
summary(mod1)

anova(mod1)

## Tukey's post-hoc test to determine which groups differ
mod1.aov <- aov(STeaLoss ~ Site*Tea, data = dat)
mod1.tuk <- TukeyHSD(mod1.aov, conf.level=0.95)

### Regression: linear relationship between two variables ###
# Note this code is provided for demonstration. n=6 is insufficient for regression analysis.
# First plot the data
temp.pl <- sdat %>% filter(Tea == "Green") %>%
                     ggplot(aes(x = Temp2_mean, y = STeaLoss_mean)) +
                     geom_point()
# linearity ok... 

## Simple linear regression
mod2 <- sdat %>% filter(Tea == "Red") %>%
        lm(STeaLoss_mean ~ Temp2_mean, data = .)

## Check remaining assumptions on residuals
# Normality
qqnorm(resid(mod2))
qqline(resid(mod2))

# Homogeneity of variances
plot(resid(mod2))

## Results
summary(mod2)


#################  
### Exercises ###
#################
## 1) Is there a difference in decomposition rate between the two types of leaf litter incubated at each site?

## 2) Is there evidence for a relationship between leaf litter decomposition and temperature?