# H3
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(a = if_else(compromise=="yes", 0, 1),
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

p2 <- h3 %>%
  filter(b == 1) %>%
  mutate(y = recode(y,
                   `DV1` = "DV: Trust",
                   `DV2` = "DV: Credibility",
                   `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
        b = recode(b,
                        `1` = "Coalition Talks Continued",
                        `0` = "Coalition Talks Stalled")) %>%
  ggplot(aes(x = y, 
             y = AME,
             color = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_minimal() +
  labs(x = "", y = "Average Marginal Effects of Compromising") +
  facet_grid(.~b) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()


df <- d %>% 
  mutate(a = if_else(compromise=="yes", 0, 1),
         b = if_else(outcome == "negotiation", 1, 0),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))
h3p <- pooled_regression(df, a, b, issue)

p2p <- h3p %>%
  filter(b == 1) %>%
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         b = recode(b,
                    `1` = "Coalition Talks Continued",
                    `0` = "Coalition Talks Stalled")) %>%
  ggplot(aes(x = y, 
             y = AME,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0,color = fig_cols[1]) +
  theme_minimal() +
  labs(x = "", y = "Average Marginal Effects of Compromising \n Pooled over Issues") +
  facet_grid(.~b) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
