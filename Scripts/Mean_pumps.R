
### table 1: mean pumps before and after the treatment, by treat
mean_pumps_1 <- df %>% 
  group_by(treatment, phase) %>% 
  summarise(m = round(mean(pumps),2), sd = round(sd(pumps), 2),
            value = paste0(m, " (", sd, ")")) %>% 
  select(treatment, phase, value) %>% 
  pivot_wider(names_from = treatment, values_from = value)

mean_pumps_tot <- df %>% 
  group_by(treatment) %>% 
  summarise(m = round(mean(pumps),2), sd = round(sd(pumps), 2),
            value = paste0(m, " (", sd, ")")) %>% 
  mutate(phase = "overall") %>% 
  select(treatment, phase, value) %>% 
  pivot_wider(names_from = treatment, values_from = value)

## table

mean_pumps_1 %>% 
  bind_rows(mean_pumps_tot) %>% 
  mutate(phase = case_when(phase == "before" ~ "Rounds 2 -- 5",
                           phase == "after"  ~ "Rounds 6 -- 10",
                           phase == "overall" ~ "All rounds"),
         phase = as.factor(phase),
         phase = fct_relevel(phase, "Rounds 2 -- 5", "Rounds 6 -- 10")) %>% 
  arrange(phase) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lcc')) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "Baseline" = 1, "Commitment" = 1)) %>% 
  add_header_above(c(" " = 1, "Mean (St.Dev.) of pumps" = 2)) %>% 
  save_kable("Tables/mean_pumps.tex")

## tests
# baseline, before-after
t.test(df$pumps[df$treatment == "Baseline"]~df$phase[df$treatment=="Baseline"]) %>% broom::tidy()

# commitment, before-after
t.test(df$pumps[df$treatment == "Temptation"]~df$phase[df$treatment=="Temptation"]) %>% broom::tidy()  

# before, across treatments
t.test(df$pumps[df$phase == "Before"]~df$treatment[df$phase == "Before"]) %>% broom::tidy() 

# after, across treatments
t.test(df$pumps[df$phase == "After"]~df$treatment[df$phase == "After"]) %>% broom::tidy()  
