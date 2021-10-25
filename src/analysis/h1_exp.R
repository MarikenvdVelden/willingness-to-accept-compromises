# H1 & H2
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(a = if_else(compromise=="yes", 0, 1),
           b = if_else(outcome == "negotiation", 1, 0)) %>%
    filter(issue == issues[i])
  if(i==1){
    h1 <- regression_direct(df, a, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_direct(df, a, b) %>%
      mutate(issue = issues[i])
    h1 <- h1 %>% add_case(tmp)
  }
}
rm (tmp)

p1 <- h1 %>%
  filter(x != "(Intercept)") %>%
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
  filter(estimate != is.na(estimate)) %>%
  ggplot(aes(x = x, 
             y = estimate,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_minimal() +
  labs(x = "", y = "Predicted Reputational Cost") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

