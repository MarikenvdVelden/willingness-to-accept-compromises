issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 1, 0)) %>%
    filter(issue == issues[i])
  if(i==1){
    me_ca <- tidy(lm(POST_1 ~ compromise + outcome +
                       factor(S1) + S2 + factor(partner) +
                  PT8 + PT1_1 + PT1_2 + PT3_2 +
                  factor(D4) + factor(D7) +
                  factor(D9) + factor(D10), df)) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- tidy(lm(POST_1 ~ compromise + outcome +
                     factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>%
      mutate(issue = issues[i])
    me_ca <- me_ca %>% add_case(tmp) 
  }
}
rm (tmp)

p_ca <- me_ca %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                    `compromise` = "Party Position: Steadfast",
                    `outcome` = "Outcome: Coalition Talks Continued",
                    `factor(S1)FDP` = "In-Party ID: FDP",
                    `factor(S1)Greens` = "In-Party ID: Greens",
                    `factor(S1)SPD` = "In-Party ID: SPD",
                    `S2` = "Partisan ID Strenght",
                    `factor(partner)die Grünen` =  "Partner: Greens",
                    `factor(partner)FDP` = "Partner: FDP",
                    `factor(partner)SPD` = "Partner: SPD"),
         term = factor(term,
                    levels = c("Partisan ID Strenght",
                               "In-Party ID: FDP","In-Party ID: Greens",
                               "In-Party ID: SPD", "Partner: Greens",
                               "Partner: FDP","Partner: SPD",
                               "Outcome: Coalition Talks Continued",
                               "Party Position: Steadfast")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  filter(estimate != is.na(estimate), term != is.na(term)) %>%
  ggplot(aes(x = term, 
             y = estimate,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Anti-Compromise Attitude") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed") +
  coord_flip()
