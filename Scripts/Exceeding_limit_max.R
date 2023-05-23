### 2. create variables to assess the limit with respect to the past
stat_before <- df %>% 
  group_by(subject, phase) %>% 
  summarise(meanpumps = mean(pumps), 
            maxpumps = max(pumps)) %>% 
  filter(phase == "before") %>% 
  select(-phase)

stat_after <- df %>% 
  group_by(subject, phase) %>% 
  summarise(meanpumps_a = mean(pumps), 
            maxpumps_a = max(pumps)) %>% 
  filter(phase == "after") %>% 
  select(-phase)

### see how the limit compares to the pumps
binding <- df %>% 
  left_join(stat_before, by = "subject") %>% 
  left_join(stat_after, by = "subject") %>% 
  filter(period == 6) %>% 
  filter(treatment == "Commitment") %>% 
  mutate(binding = maxpumps - limit,
         m_binding = meanpumps - limit) %>% 
  mutate(bound = case_when(binding > 0 ~ "strict",
                           binding == 0 ~ "weak",
                           binding < 0 ~ "not"),
         m_bound = case_when(m_binding > 0 ~ "strict",
                             m_binding == 0 ~ "weak",
                             m_binding < 0 ~ "not"),) %>% 
  mutate(harshness = 1 - limit/maxpumps) %>% 
  mutate(saturation = maxpumps_a/limit)

## how many subjects set themselves a limit? 35.1%
binding %>% 
  select(subject, limit_requested) %>% 
  distinct() %>% 
  group_by(limit_requested) %>% 
  tally() %>% 
  mutate(share = n/sum(n))


## within those, for how many was the limit binding w.r.t. max(pumps)?  42.5%
binding %>% 
  group_by(bound) %>% 
  tally() %>% 
  filter(!is.na(bound)) %>% 
  mutate(share = n/sum(n))

## within those, for how many was the limit binding w.r.t. mean(pumps)?  20.1%
binding %>% 
  group_by(m_bound) %>% 
  tally() %>% 
  filter(!is.na(m_bound)) %>% 
  mutate(share = n/sum(n))

## some stats about demand for commitment. Who are those people? 

## requested vs not requested

binding <- binding %>% 
  mutate(limbehavior = case_when(limit_requested == F ~ "not requested", 
                                 limit_requested == T ~ "requested"))
  #mutate(limbehavior = case_when(limit_requested == F ~ "not requested", 
  #                               limit_requested == T & bound == "not" ~ "not binding",
  #                               limit_requested == T & bound == "weak" ~ "weakly binding",
  #                               limit_requested == T & bound == "strict" ~ "strictly binding"))
  
binding %>% 
  group_by(limbehavior) %>% 
  summarise(age = mean(age, na.rm = T), quest = mean(gambling_quest, na.rm = T), share_m = 1 - mean(female, na.rm = T))

## are things different
paired_plus_cohen(binding, "age", "limbehavior")
paired_plus_cohen(binding, "female", "limbehavior")
paired_plus_cohen(binding, "gambling_quest", "limbehavior")
paired_plus_cohen(binding, "type", "limbehavior")

chisq.test(binding$female, binding$limbehavior) %>% tidy()
chisq.test(binding$profession, binding$limbehavior) %>% tidy()

