library(patchwork)

sat <- df %>% 
  filter(case == "Soft commitment" | case == "Hard commitment") %>% 
  group_by(subject) %>% 
  mutate(limit = last(limit)) %>% 
  select(subject, period, pumps, limit, case, phase) %>% 
  mutate(phase = as.factor(phase),
         phase = fct_recode(phase, "Rounds 2 -- 5" = "before",
                            "Rounds 6 -- 10" = "after")) %>% 
  mutate(phase = fct_relevel(phase, "Rounds 2 -- 5")) %>% 
  mutate(case = as.factor(case), 
         case = fct_relevel(case, "Soft commitment")) %>% 
  group_by(subject, case, phase) %>% 
  summarise(limit = last(limit), 
            meanpump = mean(pumps)) %>% 
  mutate(saturation = meanpump/limit,
         saturation_trunc = if_else(saturation > 1, 1, saturation)) %>% 
  arrange(subject)


## raincloud plot 
plot_sat <- sat %>% 
  filter(saturation <= 2) %>% 
  ggplot()+
  aes(phase, saturation, fill = case)+
  geom_hline(yintercept = 1, color = "black", linetype = "dashed")+
  gghalves::geom_half_violin(side = "r", alpha = 0.3, position = position_nudge(x=.14))+
  geom_point(position = position_jitter(width = 0.1), shape = 21, alpha = .3, size = 2)+
  facet_grid(.~case, scales = "free")+
  hrbrthemes::theme_ipsum_ps()+
  scale_fill_brewer(name = "", palette = "Set1", direction = 1)+
  labs(x = "", y = "Saturation ratio", title = "Saturation ratio")+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic", size = 16), 
        plot.title.position = "plot")


plot_sat_trunc <- sat %>% 
  filter(saturation_trunc <= 2) %>% 
  ggplot()+
  aes(phase, saturation_trunc, fill = case)+
  geom_hline(yintercept = 1, color = "black", linetype = "dashed")+
  gghalves::geom_half_violin(side = "r", alpha = 0.3, position = position_nudge(x=.14))+
  geom_point(position = position_jitter(width = 0.1), shape = 21, alpha = .3, size = 2)+
  facet_grid(.~case, scales = "free")+
  hrbrthemes::theme_ipsum_ps()+
  scale_fill_brewer(name = "", palette = "Set1", direction = 1)+
  labs(x = "", y = "Truncated saturation ratio", title = "Truncated saturation ratio")+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic", size = 16), 
        plot.title.position = "plot")


plot_sat/plot_sat_trunc
ggsave("Figures/saturation.png", width = 16/1.3, height = 12/1.3, units = "in", dpi = 320) 
####



# mean and st.dev by subjects, before_after
tab3 <- sat %>% 
  group_by(phase, case) %>% 
  summarise(s = mean(saturation), st = mean(saturation_trunc),
            sds = sd(saturation), stds = sd(saturation_trunc)) %>% 
  mutate(across(starts_with('s'), round, 2)) %>% 
  mutate(saturation = paste0(s, ' (', sds, ')'),
         saturation_trunc = paste0(st, ' (', stds, ')')) %>% 
  select(-sds, -stds)

tab3 <- tab3 %>% 
  select(-s, -st) %>% 
  pivot_longer(starts_with("sat"), names_to = "indicator", values_to = "value") %>% 
  arrange(case, phase) %>% 
  pivot_wider(names_from = phase, values_from = value) %>% 
  arrange(indicator, case) %>% 
  mutate(Difference = round(as.numeric(substr(`Rounds 6 -- 10`, 1,4)) - as.numeric(substr(`Rounds 2 -- 5`, 1,4)),2))

tab3 %>% 
  select(-indicator) %>% 
  mutate(case = as.factor(case), 
         case = fct_recode(case, "Soft commit" = "Soft commitment", "Hard commit" = "Hard commitment")) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  pack_rows("Saturation ratio", 1, 2) %>% 
  pack_rows("Truncated saturation ratio", 3, 4) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "Rounds 2 -- 5" = 1, "Rounds 6 -- 10" = 1, "Difference" = 1)) %>% 
  save_kable("Tables/Saturation.tex") 



## how many people go "up" towards the constraint"?
tab4 <- sat %>% 
  group_by(subject, case, phase) %>% 
  summarise(m = mean(saturation)) %>% 
  pivot_wider(names_from = phase, values_from = m) %>% 
  mutate(diff = `Rounds 6 -- 10` - `Rounds 2 -- 5`,
         sign = case_when(`Rounds 2 -- 5` > `Rounds 6 -- 10`  ~ "reduction",
                          `Rounds 2 -- 5` == `Rounds 6 -- 10` ~ "same",
                          `Rounds 2 -- 5` < `Rounds 6 -- 10` ~ "increase")) %>% 
  group_by(case, sign) %>% 
  summarise(`Mean change with respect to limit` = mean(diff), n = n()) %>% 
  mutate(`Share of subjects` = n/sum(n))


tab4 %>% 
  ungroup() %>% 
  select(-n) %>% 
  mutate(`Mean change with respect to limit` = paste0(round(100*`Mean change with respect to limit`,2),"%"),
         `Share of subjects` = paste0(round(100*`Share of subjects`,2),"%")) %>% 
  mutate(sign = str_to_sentence(sign)) %>% 
  select(sign, `Share of subjects`, everything()) %>% 
  mutate(sign = as.factor(sign), 
         sign = fct_recode(sign, "No change" = "Same"), 
         sign = fct_relevel(sign, "Reduction", "No change")) %>% 
  mutate(`Mean change with respect to limit` = case_when(`Mean change with respect to limit` == "0%" ~ "--",
                                                         TRUE ~ `Mean change with respect to limit`)) %>% 
  arrange(case, sign) %>% 
  select(-case) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  pack_rows("Soft commit", 1, 3) %>% 
  pack_rows("Hard commit", 4, 6) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "Share of subjects" = 1, "Mean change w.r.t. limit" = 1)) %>% 
  save_kable("Tables/Saturation_extra.tex") 

# same as tab4 but by status with respect to the limit before the limit is set
tab5 <- sat %>% 
  group_by(subject, case, phase) %>% 
  summarise(m = mean(saturation)) %>% 
  pivot_wider(names_from = phase, values_from = m) %>% 
  mutate(behphase1 = case_when(`Rounds 2 -- 5` < 1 ~ "Non-biting",
                               `Rounds 2 -- 5` >= 1 ~ "Biting")) %>% 
  mutate(diff = `Rounds 6 -- 10` - `Rounds 2 -- 5`,
         sign = case_when(`Rounds 2 -- 5` > `Rounds 6 -- 10`  ~ "reduction",
                          `Rounds 2 -- 5` == `Rounds 6 -- 10` ~ "same",
                          `Rounds 2 -- 5` < `Rounds 6 -- 10` ~ "increase")) %>% 
  mutate(behphase1 = as.factor(behphase1),
         sign = as.factor(sign)) %>% 
  group_by(case, behphase1, sign, .drop = F) %>% 
  summarise(m = mean(diff), n = n()) %>% 
  mutate(s = n/sum(n)) %>% 
  mutate(N = sum(n)) %>% 
  group_by(case) %>%
  mutate(share= N/(sum(N)/3)) %>% 
  mutate(behphase1 = paste0(as.character(behphase1), " (", 100*round(share,2), "%)")) %>% 
  select(-N, -share)

tab5 %>% 
  ungroup() %>% 
  select(-n) %>% 
  mutate(sign = str_to_sentence(sign),
         s = paste0(round(100*s,2),"%")) %>% 
  select(sign, behphase1, s, everything()) %>% 
  mutate(sign = as.factor(sign), 
         sign = fct_recode(sign, "No change" = "Same"), 
         sign = fct_relevel(sign, "Reduction", "No change")) %>% 
  mutate(m = if_else(m == 0 | is.nan(m),"--", as.character(round(m,2))),
         s = if_else(s == "0%", "--", s)) %>% 
  select(case, behphase1, sign, everything()) %>% 
  arrange(case, behphase1, sign) %>% 
  group_by(case, behphase1) %>% 
  mutate(n = row_number()) %>% 
  mutate(behphase1 = if_else(n != 2, " ", as.character(behphase1))) %>% 
  ungroup() %>% 
  select(-case, -n) %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  pack_rows("Soft commitment", 1, 6) %>% 
  pack_rows("Hard commitment", 7, 12) %>% 
  #collapse_rows(columns = 1, valign = "middle", latex_hline = "major", row_group_label_position = "stack") %>% 
  kable_styling(full_width = T) %>% 
  column_spec(1, width = "4cm") %>% 
  add_header_above(c("Nature of the limit" = 1, "Change in behavior", "Share of subjects" = 1, "Mean change" = 1)) %>% 
  save_kable("Tables/Saturation_extra2.tex") 

