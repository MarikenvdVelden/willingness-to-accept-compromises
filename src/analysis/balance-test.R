covs <- d %>%
  mutate(treat = if_else(outcome == "stalled" & compromise == "yes", 1,
                 if_else(outcome == "stalled" & compromise == "no", 2,
                 if_else(outcome == "negotiation" & compromise == "yes", 3, 4)))) %>%
  select(treat, D4, D7:PT1_2, PT3_1, PT3_2, pol_know:PT8, MC, POST) %>%
  drop_na()

balanced <-bal.tab(treat ~ factor(D4) + D7 + D8 + factor(D9) +
                     factor(D10) + factor(PT1_1) + factor(PT1_2) +
                    PT3_1 + PT3_2 + pol_know + PT7 + PT8 +
                    MC + POST,
                   data = covs,
                   thresholds = c(m = 0.05))[[1]]
df <- balanced %>%
  mutate(variable = c("Urbanness: Big City", "Urbanness: Countryside", "Urbanness: Middlesized City", 
                      "factor(D4)_NA","Urbanness: Rural Village", "Urbanness: Suburb",
                      "Employment", "Income", "Living Place: Badem-Würtemberg",
                      "Living Place: Bavaria", "Living Place: Berlin",
                      "Living Place: Brandenburg", "Living Place: Bremen",
                      "Living Place: Hamburg", "Living Place: Hessen",
                      "Living Place: Lower Saxony", "Living Place: Mecklenburg-Western Pomerania",
                      "Living Place: North Rhine-Westphalia", "Living Place: Rhineland-Palatinate",
                      "Living Place: Saarland", "Living Place: Saxony", "Living Place: Saxony-Anholt",
                      "Living Place: Schleswig-Holstein", "Living Place: Thuringia",
                      "Birth Place: Badem-Würtemberg",
                      "Birth Place: Bavaria", "Birth Place: Berlin",
                      "Birth Place: Brandenburg", "Birth Place: Bremen",
                      "Birth Place: Hamburg", "Birth Place: Hessen",
                      "Birth Place: Lower Saxony", "Birth Place: Mecklenburg-Western Pomerania",
                      "factor(D10)_NA","Birth Place: North Rhine-Westphalia", "Birth Place: Not born in Germany", 
                      "Birth Place: Rhinelad-Palatinate", 
                      "Birth Place: Saarland", "Birth Place: Saxony", "Birth Place: Saxony-Anholt",
                      "Birth Place: Schleswig-Holstein", "Birth Place: Thuringia",
                      "Position: Speed Limit", "Position: Top Tax", 
                      "Salience: Speed Limit", "Salience: Top Tax", 
                      "Political Knowledge", "Political Interest",
                      "Ideology", "Manipulation Checks", "Populist Attitudes")) %>%
  filter(variable != "factor(D4)_NA") %>%
  filter(variable != "factor(D10)_NA ") %>%
  mutate(variable = factor(variable,
                           levels = c("Urbanness: Big City", "Urbanness: Countryside", "Urbanness: Middlesized City", 
                                      "Urbanness: Rural Village", "Urbanness: Suburb",
                                      "Employment", "Income", "Living Place: Badem-Würtemberg",
                                      "Living Place: Bavaria", "Living Place: Berlin",
                                      "Living Place: Brandenburg", "Living Place: Bremen",
                                      "Living Place: Hamburg", "Living Place: Hessen",
                                      "Living Place: Lower Saxony", "Living Place: Mecklenburg-Western Pomerania",
                                      "Living Place: North Rhine-Westphalia", "Living Place: Rhineland-Palatinate",
                                      "Living Place: Saarland", "Living Place: Saxony-Anholt", "Living Place: Saxony",
                                      "Living Place: Schleswig-Holstein", "Living Place: Thuringia",
                                      "Birth Place: Badem-Würtemberg",
                                      "Birth Place: Bavaria", "Birth Place: Berlin",
                                      "Birth Place: Brandenburg", "Birth Place: Bremen",
                                      "Birth Place: Hamburg", "Birth Place: Hessen",
                                      "Birth Place: Lower Saxony", "Birth Place: Mecklenburg-Western Pomerania",
                                      "Birth Place: North Rhine-Westphalia", "Birth Place: Rhinelad-Palatinate",
                                      "Birth Place: Saarland", "Birth Place: Saxony", "Birth Place: Saxony-Anholt",
                                      "Birth Place: Schleswig-Holstein", "Birth Place: Thuringia",
                                      "Birth Place: Not born in Germany",
                                      "Position: Speed Limit", "Position: Top Tax", 
                                      "Salience: Speed Limit", "Salience: Top Tax", 
                                      "Political Knowledge", "Political Interest",
                                      "Ideology", "Populist Attitudes", "Manipulation Checks")),
         difference = Corr.Un,2) %>%
  select(variable, difference) %>%
  drop_na() %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above"))) %>%
  ggplot(aes(x = variable, y = difference, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("above"=fig_cols[3],
                                "below"=fig_cols[2])) +
  
  theme_bw() +
  labs(x="", y= "Standardized Mean Differences") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed") +
  coord_flip()

rm(covs, balanced)
