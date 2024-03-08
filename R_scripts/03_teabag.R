### Calculate decomposition constant from teabag incubations ###

library(here)
library(tidyverse)

####################
### Read in data ###
####################
dat <- read.csv(here("data", "ENSC120_teadecompEX.csv"))

#######################
### Incubation time ###
#######################
## Format dates
dat <- dat %>% mutate(across(contains("Date"), \(x) as.Date(x,  format = "%m/%d/%y")))

## Calculate duration of incubation in days
dat <- dat %>% mutate(inctime_d  = as.integer(difftime(Date_retrieved, Date_buried)))

##########################
### Decomposition rate ###
##########################
## Correct for mass of tea bag
# Mass (g) of empty tea bag w/ string & label
mbag <- 0.25

# Corrected masses
dat <- dat %>% mutate(init_mass_corr = Init_mass_g - mbag) %>%
               mutate(final_mass_corr = Final_massdry_g - mbag)

## Decomposition rate (day^-1)
# k = ln(Mt/M0)/t
dat <- dat %>% mutate(mass_remain = final_mass_corr/init_mass_corr) %>%
               mutate(k_d = log(mass_remain)/inctime_d)
