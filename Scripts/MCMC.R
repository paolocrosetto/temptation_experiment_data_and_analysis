
## density plot

mcmc <- read_csv("Data/MCMC.csv")

mcmc <- mcmc %>% 
  rename("Soft commit" = `Limit not applied`, 
         "Hard commit" = `Limit applied`,
         "Soft commit counterfactual" = `Limit applied ex-post`
  ) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "values") %>% 
  mutate(group = as.factor(group),
         group = fct_relevel(group, "Baseline", "Limit refused", "Soft commit")) 

## label data set
labs <- mcmc %>% 
  group_by(group) %>% 
  summarise(xPos = median(density(values)$x),
            yPos = max(density(values)$y))

mcmc %>% 
  ggplot(aes(values, fill = group))+
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")+
  geom_density(alpha = .3, size = .2, outline.type = "upper")+
  geom_label(data = labs, aes(x = xPos, y = yPos, label = group, color = group), 
                            fill = "white", hjust = 0.5, nudge_y = 0.2)+
  scale_fill_brewer(name = "", palette = "Set1", direction = -1)+
  scale_color_brewer(name = "", palette = "Set1", direction = -1)+
  labs(x = "Mean difference in pumps in rounds 6-10 from rounds 2-5",
       y = "Density")+
  hrbrthemes::theme_ipsum()+
  theme(panel.grid.minor = element_blank(), 
        legend.position = "none")
ggsave("Figures/kde_mcmc.png", width = 16/1.9, height = 12/1.9, units = "in", dpi = 320)


## credible interval (manually inputted from the text of Paul's analysis) <- this is bad but for the time being ok
credint <- tibble(groups = levels(mcmc$group),
                  mean = c(-0.16, 0.63, -0.60, -1.04, -2.16),
                  credint = c("[-0.38, 0.05]", "[0.34, 0.91]", "[-1.03, -0.18]", "[-1.75, -0.32]", "[-2.59, -1.75]"))


#table6
credint %>% 
  kbl(format = "latex", booktabs = T, col.names = NULL, align = c('lccc')) %>% 
  kable_styling(full_width = T, ) %>% 
  add_header_above(c(" " = 1, "Mean difference" = 1, "95% credible interval" = 1)) %>% 
  column_spec(1, width = "4.2cm") %>% 
  save_kable("Tables/Table_credint.tex")  

