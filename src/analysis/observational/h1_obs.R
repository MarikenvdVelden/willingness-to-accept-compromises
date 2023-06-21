analysis <- d %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income, country,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income) %>%
  mutate(country = factor(country))
analysis <- within(analysis, country <- relevel(country, ref = "The Netherlands"))
m <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                 rile_selfplacement + factor(gov_performance) + 
                 factor(pid) + factor(gender) + age +
                 education + income + factor(country) + 
                 factor(missing_trust) + factor(missing_wtac) +
                 factor(missing_rile_selfplacement) + factor(missing_pid) +
                 factor(missing_gender) + factor(missing_age) + 
                 factor(missing_education) + factor(missing_income),
               data = analysis))

h1 <- m %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                    `wtac` = "Anti-Compromise Attitude",
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
                               "Anti-Compromise Attitude")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Political Trust") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()


r1 <- m[11:15,] %>% 
  mutate(term = recode(term,
                       `factor(country)Austria` = "Austria",
                       `factor(country)France` = "France",
                       `factor(country)Germany` = "Germany",
                       `factor(country)Great Britain` = "Great Britain",
                       `factor(country)Italy` = "Italy"),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower) %>%
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  theme_ipsum() +
  labs(x = "",
       y = "Predicted Political Trust\n Country Differences: The Netherlands is Reference Category") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

r2 <- m[16:23,] %>% 
  mutate(term = recode(term,
                       `factor(missing_trust)1` = "Trust",
                       `factor(missing_wtac)1` = "Anti-Compromise Attitude",
                       `factor(missing_rile_selfplacement)1` = "Left-Right Self-Placement",
                       `factor(missing_pid)1` = "Partisan ID",
                       `factor(missing_gender)1` = "Gender",
                       `factor(missing_age)1` = "Age",
                       `factor(missing_education)1` = "Education",
                       `factor(missing_income)1` = "Income"),
         term = factor(term,
                       levels = c("Income", "Education", "Age", "Gender",
                                  "Partisan ID", "Left-Right Self-Placement",
                                  "Anti-Compromise Attitude", "Trust")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower) %>%
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  theme_ipsum() +
  labs(x = "",
       y = "Predicted Political Trust\n Missing Values") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

analysis <- d %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income, country,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income,
                missing_age) %>%
  mutate(country = factor(country)) %>%
  filter(missing_education == 0,
         missing_gender == 0)

analysis <- within(analysis, country <- relevel(country, ref = "The Netherlands")) 
m <- tidy(stats::lm(formula = trust ~ wtac + factor(political_interest) +
                 rile_selfplacement + factor(gov_performance) + 
                 factor(pid) + factor(gender) + age +
                 education + income + factor(country) +
                   factor(missing_wtac) + factor(missing_rile_selfplacement) +
                   factor(missing_pid) + factor(missing_trust), data = analysis))
r3 <- m %>% 
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       `wtac` = "Anti-Compromise Attitude",
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
                                  "Anti-Compromise Attitude")),
         lower = estimate - (1.56 * std.error),
         upper = estimate + (1.56 * std.error)) %>%
  select(term, estimate, upper, lower) %>%
  filter(term != is.na(term)) %>% 
  ggplot(aes(x = term, 
             y = estimate,
             ymin = lower,
             ymax = upper)) +
  geom_point(position = position_dodge(.5), 
             color = fig_cols[1]) + 
  geom_errorbar(position = position_dodge(.5), width = 0, 
                color = fig_cols[1]) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Political Trust") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  #scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()
  