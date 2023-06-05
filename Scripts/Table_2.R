
check <- binding %>% 
  filter(!is.na(limit)) %>% 
  select(harshness, bound)


## maximum 

table2 <- binding %>% 
  select(subject, limit, maxpumps, maxpumps_a, hard) %>% 
  filter(!is.na(limit)) %>% 
  rename(maxpumps_b = maxpumps) %>% 
  pivot_longer(starts_with("max"), names_to = c("indicator", "phase"), 
               names_sep = "_", values_to = "values") %>% 
  mutate(case = case_when(values > limit ~ "...above...",
                          values == limit ~ "...at...",
                          values < limit ~ "...below...")) %>% 
  mutate(hard = as.factor(hard)) %>% 
  group_by(phase, hard, case, .drop = F) %>% 
  tally() %>% 
  mutate(share = 100*round(n/sum(n),4)) %>% 
  select(-n) %>% 
  spread(case, share, fill = 0) %>% 
  mutate(phase = as.factor(phase),
         phase = fct_recode(phase, "Rounds 2 -- 5" = "b","Rounds 6 -- 10" = "a"),
         hard = as.factor(hard), 
         hard = fct_recode(hard, "Soft commit" = "0", "Hard commit" = "1")) %>% 
  select(phase, hard, `...below...`, `...at...`, `...above...`)

table2 %>% 
  ungroup() %>% 
  mutate(hard = fct_relevel(hard, "Soft commit"),
         phase = fct_relevel(phase, "Rounds 2 -- 5")) %>% 
  arrange(phase, hard) %>% 
  select(-phase) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  pack_rows("Periods 2--5", 1, 2) %>% 
  pack_rows("Periods 6--10", 3, 4) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "...below..." = 1, "...at..." = 1, "...above..." = 1)) %>% 
  add_header_above(c(" " = 1, "Share of subjects who pump ... the self-imposed limit" = 3)) %>% 
  save_kable("Tables/tab_tresh_max.tex") 

## testing if differences exist
testme <- binding %>% 
  select(subject, limit, maxpumps, maxpumps_a, hard) %>% 
  filter(!is.na(limit)) %>% 
  rename(maxpumps_b = maxpumps) %>% 
  pivot_longer(starts_with("max"), names_to = c("indicator", "phase"), 
               names_sep = "_", values_to = "values") %>% 
  mutate(case = case_when(values > limit ~ "up",
                          values == limit ~ "at",
                          values < limit ~ "down")) %>% 
  mutate(hard = as.factor(hard))

## chisq test, soft vs hard BEFORE setting the limit
table(testme$hard[testme$phase=="b"], testme$case[testme$phase=="b"]) %>% 
  chisq.test() %>% 
  tidy()

## chisq test, soft vs hard AFTER setting the limit
table(testme$hard[testme$phase=="a"], testme$case[testme$phase=="a"]) %>% 
  chisq.test() %>% 
  tidy()

## chisq test, before vs after for subjects with HARD commit
table(testme$phase[testme$hard==1], testme$case[testme$hard==1]) %>% 
  chisq.test() %>% 
  tidy()

## chisq test, soft vs hard AFTER setting the limit
table(testme$phase[testme$hard==0], testme$case[testme$hard==0]) %>% 
  chisq.test() %>% 
  tidy()

## mean

table2bis <- binding %>% 
  select(subject, limit, meanpumps, meanpumps_a, hard) %>% 
  filter(!is.na(limit)) %>% 
  rename(meanpumps_b = meanpumps) %>% 
  pivot_longer(starts_with("mean"), names_to = c("indicator", "phase"), 
               names_sep = "_", values_to = "values") %>% 
  mutate(case = case_when(values > limit ~ "...above...",
                          values == limit ~ "...at...",
                          values < limit ~ "...below...")) %>% 
  mutate(hard = as.factor(hard)) %>% 
  group_by(phase, hard, case, .drop = F) %>% 
  tally() %>% 
  mutate(share = 100*round(n/sum(n),4)) %>% 
  select(-n) %>% 
  spread(case, share, fill = 0) %>% 
  mutate(phase = as.factor(phase),
         phase = fct_recode(phase, "Rounds 2 -- 5" = "b","Rounds 6 -- 10" = "a"),
         hard = as.factor(hard), 
         hard = fct_recode(hard, "Soft commit" = "0", "Hard commit" = "1")) %>% 
  select(phase, hard, `...below...`, `...at...`, `...above...`)

table2bis %>% 
  ungroup() %>% 
  mutate(hard = fct_relevel(hard, "Soft commit"),
         phase = fct_relevel(phase, "Rounds 2 -- 5")) %>% 
  arrange(phase, hard) %>% 
  select(-phase) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  pack_rows("Periods 2--5", 1, 2) %>% 
  pack_rows("Periods 6--10", 3, 4) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "...below..." = 1, "...at..." = 1, "...above..." = 1)) %>% 
  add_header_above(c(" " = 1, "Share of subjects who pump ... their previous mean" = 3)) %>% 
  save_kable("Tables/tab_tresh_mean.tex") 
