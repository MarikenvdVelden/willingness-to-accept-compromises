load(here("data/intermediate/observational_data.Rdata"))
df <- d %>% 
  filter(country %in% c("The Netherlands", "Great Britain")) %>% 
  select(country, 
         trust,
         swd = satisfaction_democracy,
         wtac,
         age, gender, education, income,
         pid, political_interest,
         rile_selfplacement, gov_performance,
         m_trust = missing_trust,
         m_wtac = missing_wtac,
         m_pid = missing_pid,
         m_education = missing_education,
         m_income = missing_income,
         m_age = missing_age,
         m_gender = missing_gender,
         m_rile_selfplacement = missing_rile_selfplacement
  ) %>% 
  mutate(swd = ifelse(swd == "Dissatisfied", 0,1))

load(here("data/intermediate/cses_round5.Rdata"))

d <- d %>% 
  mutate(political_interest = recode(political_interest,
                                     `1` = "Interested",
                                     `2` = "Interested",
                                     `3` = "Not Interested",
                                     `4` = "Not Interested"),
         gov_performance = recode(gov_performance,
                                  `1` = "Satisfied",
                                  `2` = "Satisfied",
                                  `3` = "Dissatisfied",
                                  `4` = "Dissatisfied"))


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

pooled <- m1 %>%
  add_case(m2) %>% 
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       `wtac` = "Anti-Compromise Attitude",
                       `political_interestNot Interested` = "Political Interest",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `gov_performanceSatisfied` = "Evaluation of Government Performance: Satisfied",
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
                                  "Anti-Compromise Attitude"))) %>% 
  filter(term == "Anti-Compromise Attitude") %>% 
  mutate(country = "Pooled Analysis")

##Per country
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
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       `wtac` = "Anti-Compromise Attitude",
                       `political_interestNot Interested` = "Political Interest",
                       `rile_selfplacement` = "Left-Right Self-Placement",
                       `gov_performanceSatisfied` = "Evaluation of Government Performance: Satisfied",
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
                                  "Anti-Compromise Attitude"))) %>% 
  filter(term == "Anti-Compromise Attitude") %>% 
  add_case(pooled)  %>% 
  mutate(country = factor(country,
                          levels = c("Switzerland","Portugal",
                                     "Norway", "The Netherlands",
                                     "Italy","Iceland",
                                     "Great Britain", "Germany",
                                     "France", "Finland",
                                     "Denmark", "Belgium", 
                                     "Austria", "Pooled Analysis"))) %>% 
  drop_na(country)


