df <- d %>% 
  select(MC1:MC2, compromise, outcome) %>% 
  mutate(MC1 = factor(MC1),
         MC2 = factor(MC2), 
         compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0))

p0 <- tidy(lm(compromise ~ MC1, df)) %>% 
  add_case(tidy(lm(outcome ~ MC2, df))) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = recode(term,
                       `MC1no compromise shown` = "Manipulation Check: Steadfast",
                       `MC2Parties enter negotiation shown` = "Manipulation Check: Outcome"),
         upper = (estimate + (1.96*std.error)),
         lower = (estimate - (1.96*std.error))) %>% 
  ggplot(aes(x = estimate, y = term,
             xmin = lower, xmax = upper)) +
  geom_point(color = fig_cols[1]) +
  geom_errorbar(color = fig_cols[1], width = 0) +
  theme_ipsum() +
  labs(y = "", x = "Predicting Success of Manipulation")


# H1 & H2

issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(a = if_else(compromise=="yes", 0, 1),
           b = if_else(outcome == "negotiation", 1, 0),
           M1 = 0,
           M1 = ifelse(MC1 == "compromise shown" & compromise == "yes", 1, M1),
           M1 = ifelse(MC1 == "no compromise shown" & compromise == "no", 1, M1),
           M2 = 0,
           M2 = ifelse(MC2 == "Parties enter negotiation shown" & outcome == "negotiation", 1, M2),
           M2 = ifelse(MC2 == "Parties do no enter negotiation shown" & outcome == "stalled", 1, M2)) %>%
    filter(issue == issues[i]) %>% 
    filter(M2 == 1 & M2 == 1) 
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

pa <- h1 %>%
  filter(x %in% c("a", "b"),
         y != "DV3") %>%
  mutate(x = recode(x,
                    `a` = "Party Position: Steadfast",
                    `b` = "Outcome: Coalition Talks Continued"),
         x = factor(x,
                    levels = c("Outcome: Coalition Talks Continued",
                               "Party Position: Steadfast")),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility"),
         y = factor(y,
                    levels = c("DV: Trust","DV: Credibility")),
         hyp = ifelse(x == "Party Position: Steadfast", "Hypothesis 1", "Hypothesis 2")) %>%
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
  facet_grid(hyp~y, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip() 

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
                            "SPD" = 1),
           M1 = 0,
           M1 = ifelse(MC1 == "compromise shown" & compromise == "yes", 1, M1),
           M1 = ifelse(MC1 == "no compromise shown" & compromise == "no", 1, M1),
           M2 = 0,
           M2 = ifelse(MC2 == "Parties enter negotiation shown" & outcome == "negotiation", 1, M2),
           M2 = ifelse(MC2 == "Parties do no enter negotiation shown" & outcome == "stalled", 1, M2)) %>%
    filter(issue == issues[i]) %>% 
    filter(M2 == 1 & M2 == 1) 
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
                          "SPD" = 1),
         M1 = 0,
         M1 = ifelse(MC1 == "compromise shown" & compromise == "yes", 1, M1),
         M1 = ifelse(MC1 == "no compromise shown" & compromise == "no", 1, M1),
         M2 = 0,
         M2 = ifelse(MC2 == "Parties enter negotiation shown" & outcome == "negotiation", 1, M2),
         M2 = ifelse(MC2 == "Parties do no enter negotiation shown" & outcome == "stalled", 1, M2)) %>%
  filter(M2 == 1 & M2 == 1) 

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

pb <- h3 %>% 
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

# H4
#principledness 2
df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT1,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1),
         M1 = 0,
         M1 = ifelse(MC1 == "compromise shown" & compromise == 0, 1, M1),
         M1 = ifelse(MC1 == "no compromise shown" & compromise == 1, 1, M1),
         M2 = 0,
         M2 = ifelse(MC2 == "Parties enter negotiation shown" & outcome == 0, 1, M2),
         M2 = ifelse(MC2 == "Parties do no enter negotiation shown" & outcome == 1, 1, M2)) %>%
  filter(M2 == 1 & M2 == 1) 

h4p <- pooled_regression_ht1(df, compromise, outcome, b, issue) %>% 
  filter(y != "DV3") %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (2)")


#principledness1
df <- d %>% 
  filter(MC3 == 1) %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT2,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1),
         M1 = 0,
         M1 = ifelse(MC1 == "compromise shown" & compromise == 0, 1, M1),
         M1 = ifelse(MC1 == "no compromise shown" & compromise == 1, 1, M1),
         M2 = 0,
         M2 = ifelse(MC2 == "Parties enter negotiation shown" & outcome == 0, 1, M2),
         M2 = ifelse(MC2 == "Parties do no enter negotiation shown" & outcome == 1, 1, M2)) %>%
  filter(M2 == 1 & M2 == 1) 

h4bp <- pooled_regression_ht2(df, compromise, outcome, b, issue) %>% 
  filter(y != "DV3") %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (1)")


pc_2a <- h4p %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
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
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", 
       y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

pc_2b <- h4bp %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
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
  labs(x = "Levels of Principledness \n (0 = Low, 4 = High)", 
       y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

# H5
df <- d %>% 
  filter(MC3 ==1) %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT3,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1),
         M1 = 0,
         M1 = ifelse(MC1 == "compromise shown" & compromise == 0, 1, M1),
         M1 = ifelse(MC1 == "no compromise shown" & compromise == 1, 1, M1),
         M2 = 0,
         M2 = ifelse(MC2 == "Parties enter negotiation shown" & outcome == 0, 1, M2),
         M2 = ifelse(MC2 == "Parties do no enter negotiation shown" & outcome == 1, 1, M2)) %>%
  filter(M2 == 1 & M2 == 1) 

h5p <- pooled_regression_ht3(df, compromise, outcome, b, issue) %>% 
  mutate(issue = "Pooled Analysis",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))

pd <- h5p %>%
  mutate(issue = factor(issue,
                        levels = c("SpeedLimit", "TopTax", "Pooled Analysis"))) %>% 
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
  labs(x = "Levels of Mutual Trust \n (0 = Low, 10 = High)", y = "Average Marginal Effects of Being Steadfast") +
  facet_grid(.~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")



