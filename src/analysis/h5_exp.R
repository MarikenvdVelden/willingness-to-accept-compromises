# H5
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 1, 0),
           b = HT3,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h5 <- regression_ht3(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht3(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    h5 <- h5 %>% add_case(tmp) %>%
      mutate(type = "Principledness (2)")
  }
}

p4 <- h5 %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         ) %>%
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_minimal() +
  labs(x = "Levels of Mutual Trust \n (0 = Low, 10 = High)", y = "Average Marginal Effects of Levels of Mutual Trust") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0),
         b = HT3,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
h5p <- pooled_regression_ht3(df, compromise, outcome, b, issue)

p4p <- h5p %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
  ) %>%
  ggplot(aes(x = b, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_line(color = fig_cols[1]) + 
  geom_ribbon(alpha = .2, fill = fig_cols[1]) +
  theme_minimal() +
  labs(x = "Levels of Mutual Trust \n (0 = Low, 10 = High) \n Pooled over Issues", y = "Average Marginal Effects of Levels of Mutual Trust") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed")
