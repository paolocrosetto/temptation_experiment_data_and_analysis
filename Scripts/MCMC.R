
## ecdf plot from Paul's version
mcmc <- read_csv("results_bayesian_paul.CSV")

mcmc %>% 
  pivot_longer(-n, names_to = "group", values_to = "values") %>% 
  arrange(n) %>% 
  ggplot(aes(n, values, color = group))+
  geom_line()

## density plot (I like it better)

mcmc <- read_csv("raw_data_treat_diff.csv")

mcmc <- mcmc %>% 
  rename("Soft commit" = `Limit not applied`, 
         "Hard commit" = `Limit applied`,
         "Soft commit counterfactual" = `Limit applied ex-post`
  ) %>% 
  pivot_longer(everything(), names_to = "group", values_to = "values") %>% 
  mutate(group = as.factor(group),
         group = fct_relevel(group, "Baseline", "Limit refused", "Soft commit")) 
mcmc %>% 
  ggplot(aes(values, fill = group))+
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")+
  geom_density(alpha = .3, size = .2, outline.type = "upper")+
  scale_fill_brewer(name = "", palette = "Set1", direction = -1)+
  labs(x = "Mean difference in pumps in rounds 6-10 from rounds 2-5",
       y = "Density")+
  hrbrthemes::theme_ipsum()+
  theme(panel.grid.minor = element_blank(), 
        legend.position = "bottom")
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

