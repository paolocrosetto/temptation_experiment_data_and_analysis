 
## data are a bloody mess: let's show it

erratic <- df %>% 
  group_by(subject) %>% 
  mutate(meanp = mean(pumps), sdp = sd(pumps)) %>% 
  select(subject, period, meanp, sdp, pumps) %>% 
  arrange(meanp) %>% 
  ungroup() %>% 
  mutate(meanpf = ntile(meanp, 4),
         sdpf = ntile(sdp,4)) %>% 
  mutate(meanpf = as.factor(meanpf), 
         meanpf = fct_recode(meanpf, "Very risk averse" = "1", "Risk averse" = "2", "Moderately risk averse" = "3", "Risk lover" = "4"),
         sdpf = as.factor(sdpf),
         sdpf = fct_recode(sdpf, "Stable" = "1", "Unstable" = "2", "Very unstable" = "3", "Erratic" = "4"))

labels <- erratic %>% 
  group_by(meanpf, sdpf) %>% 
  summarise(n = paste0("N = ", floor(n()/9)))

erratic %>% 
  ggplot()+
  aes(x = as.factor(period), y = pumps, group = subject)+
  geom_line(alpha = .3, color = "grey20")+
  geom_label(data = labels, aes(x = 1, y = 62, label = n), inherit.aes = F, hjust = 0)+
  facet_grid(sdpf~meanpf)+
  labs(x = "Round", y = "Pumps")+
  theme_ipsum()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey95"),
        strip.text = element_text(face = "italic", size = 14))
ggsave("Figures/erratic.png", width = 16/1.2, height = 12/1.2, units = "in", dpi = 320)