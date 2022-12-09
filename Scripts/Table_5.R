
## creating the 4 different subject groups
groups <- df %>% 
  mutate(situation = case_when(treatment == "Baseline" ~ "Baseline",
                               treatment == "Temptation" & is.na(limit) ~ "Limit refused",
                               treatment == "Temptation" & !is.na(limit) & hard == 0 ~ "Soft commit",
                               treatment == "Temptation" & !is.na(limit) & hard == 1 ~ "Hard commit")) %>% 
  mutate(situation = last(situation)) %>% 
  select(subject, treatment, situation, phase, limit, period, pumps)


## creating the last counterfactual group
extragroup <-  groups %>% 
  filter(situation == "Soft commit") %>% 
  mutate(pumps = if_else(period>5 & pumps>limit, limit, pumps)) %>%               # applying the limit
  mutate(situation = "Soft commit counterfactual")   %>% 
  mutate(subject = paste0(subject,"extra")) %>% 
  select(subject, treatment, situation, phase, period, pumps)

## all groups together
groups <- groups %>% 
  bind_rows(extragroup)

means <- groups %>% 
  arrange(subject, period) %>% 
  group_by(subject, treatment, situation, phase) %>% 
  summarise(mp = mean(pumps)) %>% 
  group_by(subject) %>% 
  select(-treatment) %>% 
  pivot_wider(names_from = phase, values_from = mp) %>% 
  mutate(diff = after-before)


means <- means %>% 
  group_by(situation) %>% 
  summarise(mb = mean(before), ma = mean(after), md = mean(diff),
            sb = sd(before),   sa = sd(after),  sdi = sd(diff)) %>% 
  mutate(across(-situation, round, 2)) %>% 
  mutate(b = paste0(mb, " (", sb, ")"),
         a = paste0(ma, " (", sa, ")"),
         d = paste0(md, " (", sdi, ")")) %>% 
  select(situation, b,a,d) %>% 
  mutate(situation = as.factor(situation),
         situation = fct_relevel(situation, "Baseline", "Limit refused", "Soft commit", "Hard commit")) %>% 
  arrange(situation)

# export table
means %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "Rounds 2 -- 5" = 1, "Rounds 6 -- 10" = 1, "Difference" = 1)) %>% 
  column_spec(1, width = "4.2cm") %>% 
  save_kable("Tables/Table_means.tex")  
