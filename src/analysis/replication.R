analysis <- d %>%
  mutate(country = factor(country))

m1 <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                      rile_selfplacement + gov_performance + 
                      factor(pid) + factor(gender) + age +
                      education + income + factor(country),
                    data = analysis)) %>% 
  mutate(y = "Political Trust")

m2 <- tidy(stats::lm(formula = swd ~ wtac + political_interest +
                       rile_selfplacement + gov_performance + 
                       factor(pid) + factor(gender) + age +
                       education + income + factor(country),
                     data = analysis)) %>% 
  mutate(y = "Satisfaction with Democracy")

rep <- m1 %>%
  add_case(m2) %>% 
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       `wtac` = "Anti-Compromise Attitude",
                       `political_interest` = "Political Interest",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `gov_performance` = "Evaluation of Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Evaluation of Government Performance",
                                  "Left-Right Self-Placement",
                                  "Political Interest",
                                  "Anti-Compromise Attitude")),
         lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>%
  select(term, estimate, upper, lower, y) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper, color = y)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Dependent Variable") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
