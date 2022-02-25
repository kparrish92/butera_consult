# 01 analysis nmc 
# This script carries out nested model comparisons to assess main effects  
# and interactions from the raw data.
# ----------------------------------------------------------------------------
# Load libraries 

library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

# Load data 

raw_data <- read.csv(here("data", "raw_data.csv"))

# Replace instances of "Southern Insular Peninsular" with "Southern Insular Spain"
# after consult
raw_data$Region = replace(raw_data$Region, raw_data$Region=="Southern Insular Peninsular", "Southern Insular Spain")

# verify it worked 
raw_data %>% 
  group_by(Region) %>% 
  summarize(n = n())
# Carry out nested model comparisons starting with a null model

mod0 = lmer(Relative.intensity ~ 1 + (1 | Speaker), data = raw_data)

region = lmer(Relative.intensity ~ 1 + Region + (1 | Speaker), 
              data = raw_data)
poa = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation + 
             (1 | Speaker), data = raw_data)
sonority = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation + 
                  Sonority +
                  (1 | Speaker), data = raw_data)
stress = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                Sonority + Stress +
                (1 | Speaker), data = raw_data)
horizontal = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                    Sonority + Stress +
                    Horizontal.position +
                    (1 | Speaker), data = raw_data)
vertical = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                    Sonority + Stress +
                  Horizontal.position +
                    Vertical.Position +
                    (1 | Speaker), data = raw_data)

region_poa = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                  Sonority + Stress +
                  Horizontal.position +
                  Vertical.Position + 
                  Region:Point.of.articulation + 
                  (1 | Speaker), data = raw_data)


region_son = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                    Sonority + Stress +
                    Horizontal.position +
                    Vertical.Position + 
                    Region:Point.of.articulation + 
                    Region:Sonority + 
                    (1 | Speaker), data = raw_data)

region_stress = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                    Sonority + Stress +
                    Horizontal.position +
                    Vertical.Position + 
                    Region:Point.of.articulation + 
                    Region:Sonority + 
                    Region:Stress +
                    (1 | Speaker), data = raw_data)

region_stress = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                       Sonority + Stress +
                       Horizontal.position +
                       Vertical.Position + 
                       Region:Point.of.articulation + 
                       Region:Sonority + 
                       Region:Stress +
                       (1 | Speaker), data = raw_data)

region_stress_son = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                       Sonority + Stress +
                       Horizontal.position +
                       Vertical.Position + 
                       Region:Point.of.articulation + 
                       Region:Sonority + 
                       Region:Stress +
                       Region:Stress:Sonority +
                       (1 | Speaker), data = raw_data)

region_poa_son = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                        Sonority + Stress +
                        Horizontal.position +
                        Vertical.Position + 
                        Region:Point.of.articulation + 
                        Region:Sonority + 
                        Region:Stress +
                        Region:Point.of.articulation:Sonority +
                        (1 | Speaker), data = raw_data)

hor_region = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                        Sonority + Stress +
                        Horizontal.position +
                        Vertical.Position + 
                        Region:Point.of.articulation + 
                        Region:Sonority + 
                        Region:Stress +
                        Region:Point.of.articulation:Sonority +
                        Horizontal.position:Region +
                        (1 | Speaker), data = raw_data)

ver_region = lmer(Relative.intensity ~ 1 + Region + Point.of.articulation +
                    Sonority + Stress +
                    Horizontal.position +
                    Vertical.Position + 
                    Region:Point.of.articulation + 
                    Region:Sonority + 
                    Region:Stress +
                    Region:Point.of.articulation:Sonority +
                    Horizontal.position:Region +
                    Vertical.Position:Region +
                    (1 | Speaker), data = raw_data)

nmc_df <- anova(mod0, region, poa, sonority, stress, horizontal, vertical, region_poa, 
      region_son, region_stress, 
      region_stress_son, region_poa_son, hor_region, ver_region)



nmc_df %>% 
  write.csv(here("data", "tidy", "nmc.csv"))

ver_region %>% 
  write_rds(here("data", "tidy", "model.rmd"))


