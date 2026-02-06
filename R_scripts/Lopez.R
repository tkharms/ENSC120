### Lopez teabag data ###

library(here)
library(tidyverse)
library(multcompView)


### Data ###
dat <- read.csv(here("data", "Tea Time Data.csv"))


dat <- dat %>% mutate(Tea.Type = gsub("\\*", "", Tea.Type)) %>% # Remove asterisk from Tea.Type
               separate(Treatment, c("plant", "location"), remove = FALSE) %>% # Separate tree & location treatments
               rename(Weight_Final = Weight._Final)
  
### Calculate decomp rate ###
## To do

### Visualize data ###
## This should be on decomp rates of mass remainin

dat %>% ggplot(aes(x = Tea.Type, y = Weight_Final, fill = Treatment)) +
          geom_boxplot(outlier.shape = NA) +
          geom_point(position=position_jitterdodge(dodge.width = 0.25))

# summary table
dat.tab <- dat %>% group_by(Tea.Type, location, plant) %>%
                   summarise(mn = mean(Weight_Final, na.rm = TRUE), sd = sd(Weight_Final, na.rm =   TRUE)) %>%
                   arrange(desc(mn))

### Analysis ###
mod1 <- lm(Weight_Final ~ Tea.Type*plant*location, data = dat)

plot(mod1)
# residuals meet the assumptions of normality & homogeneous variances

summary(mod1)
anova(mod1)

## Tukey's post-hoc test to determine which groups differ
mod1.aov <- aov(Weight_Final ~ Tea.Type*plant*location, data = dat)
mod1.tuk <- TukeyHSD(mod1.aov, conf.level=0.95)

# letters
tukey.cld <- multcompLetters4(mod1.aov, mod1.tuk)
cld <- as.data.frame.list(tukey.cld$`Tea.Type:plant:location`)
dat.tab$Tukey <- cld$Letters

write.csv(dat.tab, here("data", "tea.summ.stats.csv"), row.names = FALSE)
