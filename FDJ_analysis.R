library(tidyverse)
library(hrbrthemes)
library(magrittr)
library(kableExtra)

### extra analyses for final push, Nov-Dec 2022

#### data import ####
df <- read_csv("Data/tentation.csv")

#### data cleaning  #####

# selecting needed variables
df <- df %>% 
  select(subject = participant.label, period = subsession.round_number, 
         pumps = player.pumps, treatment = player.treatment, 
         tempt = player.tentation, limit = player.tentation_limite,
         hard = player.tentation_appliquee)

# treatment, period as a nice variable
df <- df %>% 
  mutate(treatment = case_when(treatment == 4 ~ "Temptation", 
                               treatment == 0 ~ "Baseline")) %>% 
  mutate(period = as.factor(period))

# making sense of things and having a nice variable to recap it all
df <- df %>% 
  group_by(subject) %>% 
  mutate(limit_requested = !is.na(last(limit)),
         limit_applied = last(hard) == 1) %>% 
  mutate(case = case_when(treatment == "Baseline" ~ "Baseline",
                          treatment == "Temptation" & limit_requested == F ~ "Limit not requested",
                          treatment == "Temptation" & limit_requested == T & limit_applied == F ~ "Soft commitment",
                          treatment == "Temptation" & limit_requested == T & limit_applied == T ~ "Hard commitment"))
  


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

