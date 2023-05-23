## FDJ paper data analysis
##
## by Paolo Crosetto

library(tidyverse)      ## R dialect used here
library(hrbrthemes)     ## plot theme
library(magrittr)       ## for extra pipes
library(kableExtra)     ## formatting and exportin nice tables
library(gtools)         ## to set up the list of combinations needed to perform pairwise tests over pairs of treatments
library(esvis)          ## to compute cohen's d effect sizes
library(broom)          ## to clean test results and coerce them into a data frame



#### data import ####
df <- read_csv("Data/temptation_data.csv")


#### data cleaning  #####

# selecting needed variables
df <- df %>% 
  select(subject = participant.label, period = subsession.round_number, 
         pumps = player.pumps, treatment = player.treatment, 
         tempt = player.tentation, limit = player.tentation_limite,
         hard = player.tentation_appliquee, 
         profession = CSP, gambling_quest = can, type = type, 
         age, female)

# treatment, period as a nice variable
df <- df %>% 
  mutate(treatment = case_when(treatment == 4 ~ "Commitment", 
                               treatment == 0 ~ "Baseline")) %>% 
  mutate(period = as.factor(period))

# making sense of things and having a nice variable to recap it all
df <- df %>% 
  group_by(subject) %>% 
  mutate(limit_requested = !is.na(last(limit)),
         limit_applied = last(hard) == 1) %>% 
  mutate(case = case_when(treatment == "Baseline" ~ "Baseline",
                          treatment == "Commitment" & limit_requested == F ~ "Limit not requested",
                          treatment == "Commitment" & limit_requested == T & limit_applied == F ~ "Soft commitment",
                          treatment == "Commitment" & limit_requested == T & limit_applied == T ~ "Hard commitment"))
  

## helper function: detailed pairwise t-test and cohen's d
paired_plus_cohen <- function(data, var1, groupvar) {
  
  formula <- as.formula(paste(var1, "~", groupvar))
  
  data[[groupvar]] <- as.factor(data[[groupvar]])
  
  grouping <- combinations(n = length(levels(data[[groupvar]])),
                           r = 2, 
                           v = levels(data[[groupvar]]), 
                           repeats.allowed = FALSE) %>% as_tibble()
  
  tests <- map2_df(grouping$V1, grouping$V2, ~tidy(t.test(formula, 
                                                          data = data %>% filter(limbehavior %in% c(.x,.y)) )))
  tests <- tests %>% bind_cols(grouping) %>% 
    select(group1 = V1, group2 = V2, estimate1, estimate2, diff = estimate, statistic, parameter, p.value)
  
  
  
  
  cohen <- data %>% 
    coh_d(formula)
  
  tests %>% 
    left_join(cohen, by = c("group1" = "limbehavior_ref", "group2" = "limbehavior_foc")) %>% 
    arrange(p.value) %>% 
    mutate(p.value = round(p.value, 3))
}


#### Step 1. Period 1 is different from all others  #####
source("Scripts/Period_1.R")  



#### Since Period 1 != other periods, drop period 1  ##

df <- df %>% 
  mutate(period = as.numeric(period)) %>% 
  filter(period != 1) %>% 
  mutate(phase = case_when(period <= 5 ~ "before", 
                           period > 5  ~ "after"))

#### Step 2. average behavior in number of pumps  #####
source("Scripts/Mean_pumps.R")  



#### Step 3. exceeding the limit or not: MAX #####
source("Scripts/Exceeding_limit_max.R")


#### Step 4. limit harshness with respect to MAXIMUM ####
source("Scripts/harshness_max.R")


  

#### Step 5. discrete below, at, above distribution table ####
source("Scripts/Table_2.R")



#### Step 6. Limit saturation ####

## I have 8 more subjects than Paul. Why?? Solve this and then you're fine. 
source("Scripts/limit_saturation.R")



#### Step 7. Why using standard inference might be wrong ####

source("Scripts/erratic_plot.R")



#### Step 8. Risk taking before-after treatment by group ####

source("Scripts/Table_5.R")


#### Step 9. Bayesian MCMC plots ####

source("Scripts/MCMC.R")

