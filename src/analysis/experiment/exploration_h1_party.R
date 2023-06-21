# H1 - per party
issues <- unique(d$issue)
party <- unique(d$S1)
for(i in 1:length(issues)){
  for(j in 1:length(party)){
    df <- d %>% 
      mutate(a = if_else(compromise=="yes", 0, 1),
             b = if_else(outcome == "negotiation", 1, 0)) %>%
      filter(issue == issues[i] &
             S1 == party[j])
    if(i==1 & j==1){
      h1_e <- regression_direct_party(df, a, b) %>%
        mutate(issue = issues[i],
               party = party[j])
    }  
    else{
      tmp <- regression_direct_party(df, a, b) %>%
        mutate(issue = issues[i],
               party = party[j])
      h1_e <- h1_e %>% add_case(tmp)
    }
  }
}
rm (tmp)

p1_e <- h1_e %>%
  filter(x != "(Intercept)",
         y != "DV3") %>%
  mutate(x = recode(x,
                    `a` = "Party Position: Steadfast",
                    `b` = "Outcome: Coalition Talks Continued",
                    `factor(S1)FDP` = "In-Party ID: FDP",
                    `factor(S1)Greens` = "In-Party ID: Greens",
                    `factor(S1)SPD` = "In-Party ID: SPD",
                    `S2` = "Partisan ID Strenght",
                    `factor(partner)die Grünen` =  "Partner: Greens",
                    `factor(partner)FDP` = "Partner: FDP",
                    `factor(partner)SPD` = "Partner: SPD"),
         x = factor(x,
                    levels = c("Partisan ID Strenght",
                               "In-Party ID: FDP","In-Party ID: Greens",
                               "In-Party ID: SPD", "Partner: Greens",
                               "Partner: FDP","Partner: SPD",
                               "Outcome: Coalition Talks Continued",
                               "Party Position: Steadfast")),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y,
                    levels = c("DV: Trust","DV: Credibility",
                               "DV: Representation"))) %>%
  filter(estimate != is.na(estimate), x != is.na(x)) %>%
  ggplot(aes(x = x, 
             y = estimate,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Reputational Cost") +
  facet_grid(party~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed") +
  coord_flip()

