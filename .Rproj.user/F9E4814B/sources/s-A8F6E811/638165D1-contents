# A script to analyze the relationship between relative intensity (RI) per 
# region/dialect of Spanish

# Libraries 

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data 

raw_data <- read.csv(here("data", "raw_data.csv"))

mod0 = lmer(Relative.intensity ~ 1 + (1 | Speaker), data = raw_data)
region = lmer(Relative.intensity ~ 1 + Region + (1 | Speaker), 
            data = raw_data)
poa = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation + 
              (1 | Speaker), data = raw_data)
region_poa = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation + 
              (1 | Speaker), data = raw_data)
sonority = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation + 
              Sonority +
              (1 | Speaker), data = raw_data)
region_poa_sonority = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
              Sonority +
              (1 | Speaker), data = raw_data)
region_sonority = lmer(Relative.intensity ~ 1 + Region*Sonority + 
                   Point.of.articulation +
                   (1 | Speaker), data = raw_data)
stress = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                  Sonority + Stress +
                  (1 | Speaker), data = raw_data)
stress_region = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                    Sonority + Stress*Region +
                    (1 | Speaker), data = raw_data)
stress_region_sonority = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                        Sonority + Stress*Region*Sonority +
                        (1 | Speaker), data = raw_data)
horizontal = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                     Sonority + Stress*Region*Sonority +
                     Horizontal.position +
                     (1 | Speaker), data = raw_data)
horizontal_sonority_region = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                        Sonority + Stress*Region*Sonority +
                        Horizontal.position*Sonority*Region +
                        (1 | Speaker), data = raw_data)

vertical = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                     Sonority + Stress*Region*Sonority +
                     Horizontal.position*Sonority*Region + 
                     Vertical.Position +
                     (1 | Speaker), data = raw_data)

vertical_sonority_region = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                        Sonority + Stress*Region*Sonority +
                        Horizontal.position*Sonority*Region + 
                        Vertical.Position*Sonority*Region +
                        (1 | Speaker), data = raw_data)

horizontal_region = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                      Sonority + Stress*Region*Sonority +
                      Horizontal.position*Region + 
                      (1 | Speaker), data = raw_data)

vertical_region = lmer(Relative.intensity ~ 1 + Region*Point.of.articulation*
                      Sonority + Stress*Region*Sonority +
                      Horizontal.position*Region + 
                      Vertical.Position*Region + 
                      (1 | Speaker), data = raw_data)

anova(mod0, region, poa, sonority, region_poa, 
      region_poa_sonority, region_sonority, stress,
      stress_region, stress_region_sonority,
      horizontal, horizontal_sonority_region,
      vertical, vertical_sonority_region,
      horizontal_region, vertical_region)

fixef(ver_vowel_3way)
summary(ver_vowel_3way)

# check residuals are normally distributed
qqnorm(residuals(ver_vowel_3way))

# residuals vs fitted plots 
plot(ver_vowel_3way, type=c("p","smooth"), col.line=1)

x <- cooks.distance(ver_vowel_3way)
plot(x)
