## testing how period 1 differs from others
t.test(df$pumps[df$period ==1], df$pumps[df$period != 1]) %>% broom::tidy()
ks.test(df$pumps[df$period ==1], df$pumps[df$period != 1])
wilcox.test(df$pumps[df$period ==1], df$pumps[df$period != 1])

## what about period 2, or all other periods? 
pairwise.t.test(df$pumps, df$period, p.adjust.method = "none")

#### plot 1: behavior before the treatment ###
# nice version for the appendix
df %>% 
  #filter(period !=1) %>% 
  group_by(treatment, period) %>% 
  summarise(m = mean(pumps), 
            sd = sd(pumps), 
            sem = sd/sqrt(n()),
            ci = qt(0.95/2 + 0.5, n()-1),
            cil = m-1*sem, 
            cih = m+1*sem) %>% 
  ggplot(aes(period, m, color = treatment))+
  geom_errorbar(aes(ymin = cil, ymax = cih), width = .1, position = position_dodge(width = 0.2))+
  geom_point(position = position_dodge(width = 0.2))+
  geom_line(aes(group = treatment), position = position_dodge(width = 0.2), alpha = .3)+
  theme_ipsum_ps()+
  scale_color_brewer(palette = "Set1", direction = -1, name = "")+
  geom_vline(xintercept = 5.5)+
  labs(x = "Round", y = "Mean pumps")+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
ggsave("Figures/pumps_by_round.png", width = 16/1.9, height = 9/1.9, units = "in", dpi = 320)


