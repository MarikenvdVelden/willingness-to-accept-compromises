# Political Knowledge
df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0),
         b = pol_know,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))

h2_e1 <- pooled_regression_ht9(df, compromise, outcome, b) %>%
  mutate(type = "Political Knowledge",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))

# Political Interest
df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0),
         b = PT7,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))

h2_e2 <- pooled_regression_ht10(df, compromise, outcome, b) %>% 
  mutate(type = "Political Interest",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))

# ideological disctance to coalition partner based on Electoral Compas
# CDU: 4.5 on -10 to 10 -> (4.5 + 10)/2 = 7
# FDP: 6 on -10 to 10 -> (6 + 10)/2 = 8
# SDP: -5.5 on -10 to 10 -> (-5.5 + 10)/2 = 2
# Greens: -2.5 on -10 to 10 -> (-2.5 + 10)/2 = 4

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 1, 0),
         partner_pos = recode(partner,
                              "CDU" = 7,
                              "die Grünen"= 4,
                              "FDP" = 8,
                              "SPD" = 2),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1),
         distance_partner = abs(PT8 - partner_pos),
         b = distance_partner)

h2_e3 <- pooled_regression_ht11(df, compromise, outcome, b) %>% 
  mutate(type = "Ideological Distance towards the Partner",
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility",
                    `DV3` = "DV: Representation"),
         y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))


mod8b <- h2_e1 %>% 
  add_case(h2_e2) %>%   
  add_case(h2_e3) %>%    
  ggplot(aes(x = b, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper,
             group = type,
             label = type)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "", y = "Average Marginal Effects of Striking a Compromise") +
  facet_grid(y~type, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

mod8 <- h2_e3 %>%   
  ggplot(aes(x = b, 
             y = AME,
             color = y,
             fill = y,
             ymin = lower,
             ymax = upper,
             group = type,
             label = type)) +
  geom_line() + 
  geom_ribbon(alpha = .2) +
  theme_ipsum() +
  labs(x = "Ideological Distance to Negotiation Partner", y = "Average Marginal Effects of Striking a Compromise") +
  facet_grid(.~y, scales = "free") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="none",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  scale_fill_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed")

## No compromise & no goverment vs Compromise & Goverrnmennt
df <- d %>% 
  mutate(group = ifelse(compromise == "no" & outcome == "stalled", "No-No", "remove"),
         group = ifelse(compromise == "yes" & outcome == "negotiation", "Yes-Yes", group)) %>% 
  filter(group != "remove")

t1 <- t.test(DV1 ~ group, df)
t2 <- t.test(DV2 ~ group, df)
t3 <-t.test(DV3 ~ group, df)

df <- tibble(
  estimate = c(t1$estimate[1], t1$estimate[2],
               t2$estimate[1], t2$estimate[2],
               t3$estimate[1], t3$estimate[2]),
  lower = c(t1$estimate[1] - (1.56 * t1$stderr),
            t1$estimate[2] - (1.56 * t1$stderr),
            t2$estimate[1] - (1.56 * t1$stderr),
            t2$estimate[2] - (1.56 * t1$stderr),
            t3$estimate[1] - (1.56 * t1$stderr),
            t3$estimate[2] - (1.56 * t1$stderr)),
  upper = c(t1$estimate[1] + (1.56 * t1$stderr),
            t1$estimate[2] + (1.56 * t1$stderr),
            t2$estimate[1] + (1.56 * t1$stderr),
            t2$estimate[2] + (1.56 * t1$stderr),
            t3$estimate[1] + (1.56 * t1$stderr),
            t3$estimate[2] + (1.56 * t1$stderr)),
  group = c("No Compromise & Not Governing", "Compromise & Governing",
            "No Compromise & Not Governing", "Compromise & Governing",
            "No Compromise & Not Governing", "Compromise & Governing"),
  y = c("DV: Trust", "DV: Trust",
        "DV: Credibility", "DV: Credibility",
        "DV: Representation", "DV: Representation")
)

df <- df %>% 
  mutate(y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))

mod9 <- df %>% 
  ggplot(aes(x = estimate, y = group,
             xmin = lower, xmax = upper,
             color = y)) +
  geom_point() +
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  facet_grid(.~y) +
  labs(x = "Difference in Group Means \n T-test", y = "") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, linewidth = .2, linetype = "dashed") 

