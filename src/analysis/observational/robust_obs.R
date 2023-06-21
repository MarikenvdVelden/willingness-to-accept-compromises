tmp <- d %>%
  filter(country == "Austria") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
aut <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income,
                 data = tmp)) %>% 
  mutate(type = "Austria")

p1 <- aut %>%
  mutate(term = recode(term,
                       `wtac` = "Willingness to Accept Compromise",
                       `factor(political_interest)Not Interested` = "Not Politically Interested",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `factor(gov_performance)Satisfied` = "Satisfied with Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Satisfied with Government Performance",
                                  "Left-Right Self-Placement",
                                  "Not Politically Interested",
                                  "Willingness to Accept Compromise")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower, type) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  facet_grid(type~.) +
  theme_ipsum() +
  labs(x = "", y = "Political Trust Predicted by Willingness to Compromise") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

tmp <- d %>%
  filter(country == "France") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
fr <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                  rile_selfplacement + factor(gov_performance) + 
                  factor(pid) + factor(gender) + age +
                  education + income,
                data = tmp))%>% 
  mutate(type = "France")

p2 <- fr %>%
  mutate(term = recode(term,
                       `wtac` = "Willingness to Accept Compromise",
                       `factor(political_interest)Not Interested` = "Not Politically Interested",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `factor(gov_performance)Satisfied` = "Satisfied with Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Satisfied with Government Performance",
                                  "Left-Right Self-Placement",
                                  "Not Politically Interested",
                                  "Willingness to Accept Compromise")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower, type) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  facet_grid(type~.) +
  theme_ipsum() +
  labs(x = "", y = "Political Trust Predicted by Willingness to Compromise") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

tmp <- d %>%
  filter(country == "Germany") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
de  <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income,
                 data = tmp)) %>% 
  mutate(type = "Germany")

p3 <- de %>%
  mutate(term = recode(term,
                       `wtac` = "Willingness to Accept Compromise",
                       `factor(political_interest)Not Interested` = "Not Politically Interested",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `factor(gov_performance)Satisfied` = "Satisfied with Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Satisfied with Government Performance",
                                  "Left-Right Self-Placement",
                                  "Not Politically Interested",
                                  "Willingness to Accept Compromise")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower, type) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  facet_grid(type~.) +
  theme_ipsum() +
  labs(x = "", y = "Political Trust Predicted by Willingness to Compromise") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

tmp <- d %>%
  filter(country == "Italy") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
it  <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income + missing_rile_selfplacement,
                 data = tmp))%>% 
  mutate(type = "Italy")

p4 <- it %>%
  mutate(term = recode(term,
                       `wtac` = "Willingness to Accept Compromise",
                       `factor(political_interest)Not Interested` = "Not Politically Interested",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `factor(gov_performance)Satisfied` = "Satisfied with Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Satisfied with Government Performance",
                                  "Left-Right Self-Placement",
                                  "Not Politically Interested",
                                  "Willingness to Accept Compromise")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower, type) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  facet_grid(type~.) +
  theme_ipsum() +
  labs(x = "", y = "Political Trust Predicted by Willingness to Compromise") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

tmp <- d %>%
  filter(country == "The Netherlands") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
nl  <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income + missing_rile_selfplacement +
                   missing_trust + missing_wtac + missing_education +
                   missing_gov_performance + missing_pid,
                 data = tmp)) %>% 
  mutate(type = "Netherlands")

p5 <- nl %>%
  mutate(term = recode(term,
                       `wtac` = "Willingness to Accept Compromise",
                       `factor(political_interest)Not Interested` = "Not Politically Interested",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `factor(gov_performance)Satisfied` = "Satisfied with Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Satisfied with Government Performance",
                                  "Left-Right Self-Placement",
                                  "Not Politically Interested",
                                  "Willingness to Accept Compromise")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower, type) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  facet_grid(type~.) +
  theme_ipsum() +
  labs(x = "", y = "Political Trust Predicted by Willingness to Compromise") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

tmp <- d %>%
  filter(country == "Great Britain") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income) %>% 
  drop_na()
gb  <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income + missing_rile_selfplacement +
                   missing_trust + missing_wtac + missing_education +
                   missing_gov_performance + missing_pid +
                   missing_income + missing_gender + missing_age,
                 data = tmp)) %>% 
  mutate(type = "Great Britain")

p6 <- gb %>%
  mutate(term = recode(term,
                       `wtac` = "Willingness to Accept Compromise",
                       `factor(political_interest)Not Interested` = "Not Politically Interested",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `factor(gov_performance)Satisfied` = "Satisfied with Government Performance",
                       `factor(pid)Partisan ID` = "Partisan ID",
                       `factor(gender)Male` = "Male",
                       `age` =  "Age",
                       `education` = "Education",
                       `income` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Male",
                                  "Partisan ID",
                                  "Satisfied with Government Performance",
                                  "Left-Right Self-Placement",
                                  "Not Politically Interested",
                                  "Willingness to Accept Compromise")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower, type) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  facet_grid(type~.) +
  theme_ipsum() +
  labs(x = "", y = "Political Trust Predicted by Willingness to Compromise") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()