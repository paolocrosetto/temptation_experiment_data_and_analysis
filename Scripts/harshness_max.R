# overall: subjects set limits that are 75% higher than their behavior
binding %>% 
  ungroup() %>% 
  summarise(m = mean(harshness, na.rm = T), 
            sd = sd(harshness, na.rm = T))

# restricted to subjects having chosen a binding limit, it is 35.3% below
binding %>% 
  group_by(bound) %>% 
  summarise(m = mean(harshness, na.rm = T),
            sd = sd(harshness, na.rm = T))

## plot for the appendix 
binding %>% 
  filter(!is.na(limit)) %>% 
  filter(bound != "weak") %>% 
  mutate(bound = if_else(bound == "not", "Not bindnging limit", "Binding limit")) %>% 
  mutate(harshness = if_else(harshness <= -1, -1, harshness)) %>% 
  ggplot()+
  aes(x = harshness, color = bound, fill = bound)+
  geom_density(trim = T, alpha =.3)+
  scale_color_brewer(name = "", palette = "Set1", direction = 1, 
                     guide = guide_legend(reverse = T))+
  scale_fill_brewer(name = "", palette = "Set1", direction = 1,
                    guide = guide_legend(reverse = T))+
  labs(x = "Limit harshness")+
  theme_ipsum_ps()+
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
ggsave("Figures/limit_harshness.png", width = 16/1.9, height = 9/1.9, units = "in", dpi = 320) 


## is there a correlation between limit harshness and questionnaire results
cor.test(binding$harshness, binding$gambling_quest)

## is there a relationship with gender
lm(harshness~ female*age, data = binding) %>% tidy()
