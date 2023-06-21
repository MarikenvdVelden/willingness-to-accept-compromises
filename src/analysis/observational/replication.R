d <- d %>% 
  add_case(df) %>% 
  filter(country != "Thailand", country != "Turkey") %>% 
  mutate(country = recode(country,
                          `Denkmark` = "Denmark",
                          `The Netherlands` = "the Netherlands",
                          `Chili` = "Chile",
                          `Brazilia` = "Brazil",
                          .default = country),
         continent = recode(country,
                            `Australia` = "Australia",
                            `Austria` = "Western Europe",
                            `Belgium` = "Western Europe",
                            `Brazil` = "South America",
                            `Canada` = "North America",
                            `Chile` = "South America",
                            `Costa Rica` = "South America",
                            `Denmark` = "Westen Europe",
                            `Finland` = "Westen Europe",
                            `France` = "Westen Europe",
                            `Germany` = "Westen Europe",
                            `Great Britain` = "Westen Europe",
                            `Hungary` = "Eastern Europe",
                            `Iceland` = "Westen Europe",
                            `Israel` = "Asia",
                            `Italy` = "Westen Europe",
                            `Japan` = "Asia",
                            `Lithuania` = "Eastern Europe",
                            `Montenegro` = "Eastern Europe",
                            `New Zealand` = "Australia",
                            `Norway` = "Westen Europe",
                            `Portugal` = "Westen Europe",
                            `Slovakia` = "Eastern Europe",
                            `Switzerland` = "Westen Europe",
                            `Taiwan` = "Asia",
                            `Iceland` = "Westen Europe",
                            `the Netherlands` = "Westen Europe",
                            `Tunesia` = "Africa",
                            `United States` = "North America"))

analysis <- d %>%
  filter(continent == "Westen Europe") %>% 
  mutate(country = factor(country))
  

m1 <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                      rile_selfplacement + gov_performance + 
                      factor(pid) + factor(gender) + age +
                      education + income + factor(country),
                    data = analysis)) %>% 
  mutate(y = "Trust in Politicians")

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


##Per country
pooled <- m1 %>% 
  add_case(m2) %>% 
  mutate(country = "Pooled Analysis") %>% 
  filter(term == "wtac") %>% 
  mutate(lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>%
  select(term, estimate, upper, lower, y, country)

u_country <- unique(analysis$country)

for(i in 2:length(u_country)){
  df <- analysis %>% 
    filter(country==u_country[i]) %>% 
    drop_na(trust, wtac, political_interest,
            rile_selfplacement, gov_performance,
            pid, gender, age, education, income)
  if(i==2 & dim(df)[1]>0){
    m1 <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                           rile_selfplacement + gov_performance + 
                           factor(pid) + factor(gender) + age +
                           education + income,
                         data = df)) %>% 
      mutate(y = "Trust in Politicians",
             country = u_country[i])
    
    m2 <- tidy(stats::lm(formula = swd ~ wtac + political_interest +
                           rile_selfplacement + gov_performance + 
                           factor(pid) + factor(gender) + age +
                           education + income,
                         data = df)) %>% 
      mutate(y = "Satisfaction with Democracy",
             country = u_country[i])
  }
  if(i>2 & dim(df)[1]>1){
    tmp <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                            rile_selfplacement + gov_performance + 
                            factor(pid) + factor(gender) + age +
                            education + income,
                          data = df)) %>% 
      mutate(y = "Trust in Politicians",
             country = u_country[i])
    m1 <- m1 %>% 
      add_case(tmp)
    
    tmp <- tidy(stats::lm(formula = swd ~ wtac + political_interest +
                            rile_selfplacement + gov_performance + 
                            factor(pid) + factor(gender) + age +
                            education + income,
                          data = df)) %>% 
      mutate(y = "Satisfaction with Democracy",
             country = u_country[i])
    
    m2 <- m2 %>% 
      add_case(tmp)
  }
}

rep2 <- m1 %>%
  add_case(m2) %>% 
  filter(term == "wtac") %>%
  mutate(lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>%
  select(term, estimate, upper, lower, y, country) %>%
  add_case(pooled) %>% 
  mutate(country = factor(country,
                          levels = c("Switzerland","Portugal",
                                     "Norway", "The Netherlands",
                                     "Italy","Iceland",
                                     "Great Britain", "Germany",
                                     "France", "Finland",
                                     "Denmark", "Belgium", 
                                     "Austria", "Pooled Analysis"))) %>% 
  drop_na(country) %>% 
  ggplot(aes(x = country, 
             y = estimate,
             ymin = lower,
             ymax = upper, color = y)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Effect of Dependent Variable for Anti-Compromise Attitude") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()

  
## Full country set
d <- d %>% 
  add_case(df) %>% 
  filter(country != "Thailand", country != "Turkey") %>% 
  mutate(country = recode(country,
                          `Denkmark` = "Denmark",
                          `The Netherlands` = "the Netherlands",
                          `Chili` = "Chile",
                          `Brazilia` = "Brazil",
                          .default = country),
         continent = recode(country,
                            `Australia` = "Australia",
                            `Austria` = "Western Europe",
                            `Belgium` = "Western Europe",
                            `Brazil` = "South America",
                            `Canada` = "North America",
                            `Chile` = "South America",
                            `Costa Rica` = "South America",
                            `Denmark` = "Westen Europe",
                            `Finland` = "Westen Europe",
                            `France` = "Westen Europe",
                            `Germany` = "Westen Europe",
                            `Great Britain` = "Westen Europe",
                            `Hungary` = "Eastern Europe",
                            `Iceland` = "Westen Europe",
                            `Israel` = "Asia",
                            `Italy` = "Westen Europe",
                            `Japan` = "Asia",
                            `Lithuania` = "Eastern Europe",
                            `Montenegro` = "Eastern Europe",
                            `New Zealand` = "Australia",
                            `Norway` = "Westen Europe",
                            `Portugal` = "Westen Europe",
                            `Slovakia` = "Eastern Europe",
                            `Switzerland` = "Westen Europe",
                            `Taiwan` = "Asia",
                            `Iceland` = "Westen Europe",
                            `the Netherlands` = "Westen Europe",
                            `Tunesia` = "Africa",
                            `United States` = "North America"))

analysis <- d %>%
  mutate(country = factor(country))

m1 <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                      rile_selfplacement + gov_performance + 
                      factor(pid) + factor(gender) + age +
                      education + income + factor(country),
                    data = analysis)) %>% 
  mutate(y = "Trust in Politicians")

m2 <- tidy(stats::lm(formula = swd ~ wtac + political_interest +
                       rile_selfplacement + gov_performance + 
                       factor(pid) + factor(gender) + age +
                       education + income + factor(country),
                     data = analysis)) %>% 
  mutate(y = "Satisfaction with Democracy")


##Per country
pooled <- m1 %>% 
  add_case(m2) %>% 
  mutate(country = "Pooled Analysis") %>% 
  filter(term == "wtac") %>% 
  mutate(lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>%
  select(term, estimate, upper, lower, y, country)

u_country <- unique(analysis$country)

for(i in 2:length(u_country)){
  df <- analysis %>% 
    filter(country==u_country[i]) %>% 
    drop_na(trust, wtac, political_interest,
            rile_selfplacement, gov_performance,
            pid, gender, age, education, income)
  if(i==2 & dim(df)[1]>0){
    m1 <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                           rile_selfplacement + gov_performance + 
                           factor(pid) + factor(gender) + age +
                           education + income,
                         data = df)) %>% 
      mutate(y = "Trust in Politicians",
             country = u_country[i])
    
    m2 <- tidy(stats::lm(formula = swd ~ wtac + political_interest +
                           rile_selfplacement + gov_performance + 
                           factor(pid) + factor(gender) + age +
                           education + income,
                         data = df)) %>% 
      mutate(y = "Satisfaction with Democracy",
             country = u_country[i])
  }
  if(i>2 & dim(df)[1]>1){
    tmp <- tidy(stats::lm(formula = trust ~ wtac + political_interest +
                            rile_selfplacement + gov_performance + 
                            factor(pid) + factor(gender) + age +
                            education + income,
                          data = df)) %>% 
      mutate(y = "Trust in Politicians",
             country = u_country[i])
    m1 <- m1 %>% 
      add_case(tmp)
    
    tmp <- tidy(stats::lm(formula = swd ~ wtac + political_interest +
                            rile_selfplacement + gov_performance + 
                            factor(pid) + factor(gender) + age +
                            education + income,
                          data = df)) %>% 
      mutate(y = "Satisfaction with Democracy",
             country = u_country[i])
    
    m2 <- m2 %>% 
      add_case(tmp)
  }
}

rep <- m1 %>%
  add_case(m2) %>% 
  filter(term == "wtac") %>%
  mutate(lower = estimate - (1.96 * std.error),
         upper = estimate + (1.96 * std.error)) %>%
  select(term, estimate, upper, lower, y, country) %>%
  add_case(pooled) %>% 
  mutate(country = factor(country,
                          levels = c("Switzerland","Portugal",
                                     "Norway", "The Netherlands",
                                     "Italy","Iceland",
                                     "Great Britain", "Germany",
                                     "France", "Finland",
                                     "Denmark", "Belgium", 
                                     "Austria", "Pooled Analysis"))) %>% 
  drop_na(country) %>% 
  ggplot(aes(x = country, 
             y = estimate,
             ymin = lower,
             ymax = upper, color = y)) +
  geom_point(position = position_dodge(.5)) + 
  geom_errorbar(position = position_dodge(.5), width = 0) +
  theme_ipsum() +
  labs(x = "", y = "Predicted Effect of Dependent Variable for Anti-Compromise Attitude") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = fig_cols) +
  geom_hline(yintercept = 0, size = .2, linetype = "dashed") +
  coord_flip()


rep2_b <- m1 %>%
  add_case(m2) %>% 
  filter(term != "(Intercept)",
         country %in% c("Costa Rica",
                        "Denmark",
                        "Finland",
                        "France",
                        "Germany")) %>%
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
  select(term, estimate, upper, lower, y, country) %>%
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
  coord_flip() +
  facet_grid(.~country, scales = "free")

rep2_c <- m1 %>%
  add_case(m2) %>% 
  filter(term != "(Intercept)",
         country %in% c("Great Britain",
                        "Hungary",
                        "Iceland",
                        "Israel",
                        "Italy")) %>%
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
  select(term, estimate, upper, lower, y, country) %>%
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
  coord_flip() +
  facet_grid(.~country, scales = "free")

rep2_d <- m1 %>%
  add_case(m2) %>% 
  filter(term != "(Intercept)",
         country %in% c("Japan",
                        "Lithuania",
                        "Montenegro",
                        "New Zealand",
                        "Norway")) %>%
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
  select(term, estimate, upper, lower, y, country) %>%
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
  coord_flip() +
  facet_grid(.~country, scales = "free")

rep2_e <- m1 %>%
  add_case(m2) %>% 
  filter(term != "(Intercept)",
         country %in% c("Portugal",
                        "Slovakia",
                        "Switzerland",
                        "Thailand",
                        "Turkey",
                        "United States")) %>%
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
  select(term, estimate, upper, lower, y, country) %>%
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
  coord_flip() +
  facet_grid(.~country, scales = "free")

