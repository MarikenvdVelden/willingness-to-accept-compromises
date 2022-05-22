#Distributuions Principledness & Mutual Trust (also per issue)
p45d1 <- d %>% 
  ggplot(aes(HT2, fill = issue)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_grid(.~issue) +
  labs(x = "Princpledness (1)", y = "") +
  scale_y_continuous(labels = scales::percent) + 
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols)

p45d2 <- d %>% 
  ggplot(aes(HT1, fill = issue)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_grid(.~issue) +
  labs(x = "Princpledness (2)", y = "") +
  scale_y_continuous(labels = scales::percent) + 
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols)

p45d3 <- d %>% 
  ggplot(aes(HT3, fill = issue)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  facet_grid(.~issue) +
  labs(x = "Mutual Trust", y = "") +
  scale_y_continuous(labels = scales::percent) + 
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols)
# Direct effect Principledness & Mutual Trust

# H4 & H5 (Princpledness 1)
issues <- unique(d$issue)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(a = if_else(compromise=="yes", 0, 1),
           b = if_else(outcome == "negotiation", 1, 0),
           c = HT2) %>%
    filter(issue == issues[i])
  if(i==1){
    h45e <- regression_direct_explor(df, a, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_direct_explor(df, a, b) %>%
      mutate(issue = issues[i])
    h45e <- h45e %>% add_case(tmp)
  }
}
rm (tmp)

h45e <- h45e %>%
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
                    `factor(partner)SPD` = "Partner: SPD",
                    `c` = "Principledness",
                    `HT3` = "Mutual Trust"),
         x = factor(x,
                    levels = c("Partisan ID Strenght",
                               "In-Party ID: FDP","In-Party ID: Greens",
                               "In-Party ID: SPD", "Partner: Greens",
                               "Partner: FDP","Partner: SPD",
                               "Mutual Trust", "Principledness",
                               "Outcome: Coalition Talks Continued",
                               "Party Position: Steadfast")),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y,
                    levels = c("DV: Trust","DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (1)") %>%
  filter(estimate != is.na(estimate), x != is.na(x)) 

# H4 & H5 (Princpledness 2)
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(a = if_else(compromise=="yes", 0, 1),
           b = if_else(outcome == "negotiation", 1, 0),
           c = HT1) %>%
    filter(issue == issues[i])
  if(i==1){
    h45eb <- regression_direct_explor(df, a, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_direct_explor(df, a, b) %>%
      mutate(issue = issues[i])
    h45eb <- h45eb %>% add_case(tmp)
  }
}
rm (tmp)

h45eb <- h45eb %>%
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
                    `factor(partner)SPD` = "Partner: SPD",
                    `c` = "Principledness",
                    `HT3` = "Mutual Trust"),
         x = factor(x,
                    levels = c("Partisan ID Strenght",
                               "In-Party ID: FDP","In-Party ID: Greens",
                               "In-Party ID: SPD", "Partner: Greens",
                               "Partner: FDP","Partner: SPD",
                               "Mutual Trust", "Principledness",
                               "Outcome: Coalition Talks Continued",
                               "Party Position: Steadfast")),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y,
                    levels = c("DV: Trust","DV: Credibility",
                               "DV: Representation")),
         type = "Principledness (2)") %>%
  filter(estimate != is.na(estimate), x != is.na(x))

p34e <- h45e %>% 
  add_case(h45eb) %>% 
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
  facet_grid(type~y) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()


## Reprentation & Mutual Trust
df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT3,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) %>%
  filter(issue == "TopTax", PT1_2 == 1)
h5ae <- lm(DV3 ~  outcome + compromise * b +
            factor(S1) + S2 + factor(partner) +
            PT8 + PT1_1 + PT3_2 +
            factor(D4) + factor(D7) +
            factor(D9) + factor(D10),
          data= df)
#ma <- summary(margins(h5ae, variables = "compromise", at = list(b = 0:10))) %>%
#  mutate(lower = AME - (1.56 * SE),
#         upper = AME + (1.56 * SE)) %>%
#  select(AME, upper, lower, b) %>%
#  mutate(position = "In favor of TopTax Increase")

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT3,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) %>%
  filter(issue == "TopTax", PT1_2 == 0)
h5be <- lm(DV3 ~  outcome + compromise * b +
            factor(S1) + S2 + factor(partner) +
            PT8 + PT1_1 + PT3_2 +
            factor(D4) + factor(D7) +
            factor(D9) + factor(D10),
          data= df)
#mb <- summary(margins(h5be, variables = "compromise", at = list(b = 0:10))) %>%
#  mutate(lower = AME - (1.56 * SE),
#         upper = AME + (1.56 * SE)) %>%
#  select(AME, upper, lower, b) %>%
#  mutate(position = "Against TopTax Increase")
