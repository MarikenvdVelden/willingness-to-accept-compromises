# H3
issues <- unique(d$issue)
party <- unique(d$S1)

for(i in 1:length(issues)){
  for(j in 1:length(party)){
    df <- d %>% 
    mutate(a = if_else(compromise=="yes", 0, 1),
           b = if_else(outcome == "negotiation", 1, 0),
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i]  &
             S1 == party[j])
  if(i==1 & j == 1){
    h3_e <- regression_party(df, a, b) %>%
      mutate(issue = issues[i],
             party = party[j])
  }  
  else{
    tmp <- regression_party(df, a, b) %>%
      mutate(issue = issues[i],
             party = party[j])
    h3_e <- h3_e %>% add_case(tmp)
    }
  }
}

h3_e <- h3_e %>%
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

d2 <- d %>% 
  mutate(a = if_else(compromise=="yes", 0, 1),
         b = if_else(outcome == "negotiation", 1, 0),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))

for(i in 1:length(party)){
  df <- d2 %>% filter(S1 == party[i])
  if(i==1){
    h3p_e <- pooled_regression_party(df, a, b, issue) %>%
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
                                   "Party Position: Compromise")),
             party = party[i])
  }
  else{
    tmp <- pooled_regression_party(df, a, b, issue) %>%
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
                                   "Party Position: Compromise")),
             party = party[i])
    h3p_e <- h3p_e %>% add_case(tmp)
  }
}

  
p2_e <- h3_e %>% 
  add_case(h3p_e) %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  ggplot(aes(x = x, 
             y = estimate,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(party~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed") +
  coord_flip()


