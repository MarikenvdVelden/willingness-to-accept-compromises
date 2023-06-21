# H3
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(a = if_else(compromise=="yes", 1, 0),
           b = if_else(outcome == "negotiation", 1, 0),
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    h3 <- regression(df, a, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression(df, a, b) %>%
      mutate(issue = issues[i])
    h3 <- h3 %>% add_case(tmp)
  }
}

h3 <- h3 %>%
 filter(x %in% c("a", "b", "a:b"),
        y != "DV3") %>%
  mutate(y = recode(y,
                   `DV1` = "DV: Trust",
                   `DV2` = "DV: Credibility",
                   `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         x = recode(x,
                    `a` = "Party Position: Compromise",
                    `b` = "Outcome: Coalition Talks Continued",
                    `a:b` = "Interaction: Party Position * Outcome"),
         x = factor(x,
                    levels = c("Interaction: Party Position * Outcome",
                               "Outcome: Coalition Talks Continued",
                               "Party Position: Compromise")))

df <- d %>% 
  mutate(a = if_else(compromise=="yes", 0, 1),
         b = if_else(outcome == "negotiation", 1, 0),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))

h3p <- pooled_regression(df, a, b, issue) %>%
  filter(x %in% c("a", "b", "a:b"),
         y != "DV3") %>%
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                             `DV1` = "DV: Trust",
                             `DV2` = "DV: Credibility",
                             `DV3` = "DV: Representation"),
                  y = factor(y, 
                             levels = c("DV: Trust", "DV: Credibility",
                                        "DV: Representation")),
         x = recode(x,
                    `a` = "Party Position: Compromise",
                    `b` = "Outcome: Coalition Talks Continued",
                    `a:b` = "Interaction: Party Position * Outcome"),
         x = factor(x,
                    levels = c("Interaction: Party Position * Outcome",
                               "Outcome: Coalition Talks Continued",
                               "Party Position: Compromise")))

p2 <- h3 %>% 
  add_case(h3p) %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis")),
         hyp = "Hypothesis 3") %>% 
  ggplot(aes(x = x, 
             y = estimate,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  facet_grid(hyp~y) +
  labs(x = "", y = "Predicted Reputational Cost") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed") +
  coord_flip()

