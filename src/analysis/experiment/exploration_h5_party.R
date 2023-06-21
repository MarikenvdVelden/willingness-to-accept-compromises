# H5
issues <- unique(d$issue)
party <- unique(d$S1)

#Mutual Trust
for(i in 1:length(issues)){
  for(j in 1:length(party)){
    df <- d %>% 
      mutate(compromise = if_else(compromise=="yes", 0, 1),
             outcome = if_else(outcome == "negotiation", 0, 1),
             b = HT3,
             partner = recode(partner, 
                              "CDU" = 1,
                              "die Grünen"= 0,
                              "FDP" = 0,
                              "SPD" = 1)) %>%
      filter(issue == issues[i] & S1 == party[j])
    if(i==1 & j==1){
      h5_e <- regression_ht3_party(df, compromise, outcome, b) %>%
        mutate(issue = issues[i],
               party = party[j])
    }  
    else{
      tmp <- regression_ht3_party(df, compromise, outcome, b) %>%
        mutate(issue = issues[i],
               party = party[j])
      h5_e <- h5_e %>% add_case(tmp) %>%
        mutate(type = "Mutual Trust")
    }
  }
}

h5_e <- h5_e %>%
  filter(y != "DV3") %>% 
  mutate(y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))

#pooled
d2 <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT3,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 

for(i in 1:length(party)){
  df <- d2 %>% filter(S1 == party[i])
  if(i==1){
    h5p_e <- pooled_regression_ht3_party(df, compromise, outcome, b, issue) %>% 
      filter(y != "DV3") %>% 
      mutate(issue = "Pooled Analysis",
             y = recode(y,
                        `DV1` = "DV: Trust",
                        `DV2` = "DV: Credibility",
                        `DV3` = "DV: Representation"),
             y = factor(y, 
                        levels = c("DV: Trust", "DV: Credibility",
                                   "DV: Representation")),
             type = "Mutual Trust",
             party = party[i])
  }
  else{
    tmp <- pooled_regression_ht3_party(df, a, b, issue) %>%
      filter(y != "DV3") %>% 
      mutate(issue = "Pooled Analysis",
             y = recode(y,
                        `DV1` = "DV: Trust",
                        `DV2` = "DV: Credibility",
                        `DV3` = "DV: Representation"),
             y = factor(y, 
                        levels = c("DV: Trust", "DV: Credibility",
                                   "DV: Representation")),
             type = "Mutual Trust",
             party = party[i])
    h5p_e <- h5p_e %>% add_case(tmp)
  }
}

#Viz
p5_e1 <- h5_e %>% 
  add_case(h5p_e) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Trust") %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Levels of Mutual Trust \n (0 = Low, 10 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Trust") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

p5_e2 <- h5_e %>% 
  add_case(h5p_e) %>% 
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
  filter(y == "DV: Credibility") %>% 
  ggplot(aes(x = b, 
             y = AME,
             color = issue,
             fill = issue,
             ymin = lower,
             ymax = upper,
             label = issue)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Levels of Mutual Trust \n (0 = Low, 10 = High)", 
       y = "Average Marginal Effects of Being Steadfast",
       subtitle = "DV: Credibility") +
  facet_grid(issue~party) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")
