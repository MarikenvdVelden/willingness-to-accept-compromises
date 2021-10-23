analysis <- d %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income, country,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income) %>%
  mutate(country = factor(country))
analysis <- within(analysis, country <- relevel(country, ref = "The Netherlands"))
m <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                 rile_selfplacement + factor(gov_performance) + 
                 factor(pid) + factor(gender) + age +
                 education + income + factor(country) + 
                 factor(missing_trust) + factor(missing_wtac) +
                 factor(missing_rile_selfplacement) + factor(missing_pid) +
                 factor(missing_gender) + factor(missing_age) + 
                 factor(missing_education) + factor(missing_income),
               data = analysis)
h1 <- ggcoefstats(
  x = tidy(m)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title = "Political Trust Predicted by Willingness to Compromise",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = F) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("Willingness to Accept Compromise", 
                              "Not Politically Interested", 
                              "Left-Right Self-Placement",
                              "Satisfied with Government Performance",
                              "Partisan ID",
                              "Male",
                              "Age",
                              "Education", "Income")) +
  labs(x = "Regression Coefficients", y = NULL) 

r1 <- ggcoefstats(
  x = tidy(m)[10:14,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  only.significant = TRUE,
  stats.label.color = "seagreen",
  title = "Political Trust Predicted by Willingness to Compromise",
  subtitle = "Country Differences: The Netherlands is Reference Category",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = FALSE) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("Austria",
                              "France", 
                              "Germany", 
                              "Great Britain",
                              "Italy")) +
  labs(x = "Regression Coefficients", y = NULL)

r2 <- ggcoefstats(
  x = tidy(m)[15:22,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  only.significant = TRUE,
  stats.label.color = "seagreen",
  title = "Political Trust Predicted by Willingness to Compromise",
  subtitle = "Missing Values",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = FALSE) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("Trust", 
                              "Willingness to Accept Compromise", 
                              "Left-Right Self-Placement",
                              "Party ID", "Gender", "Age",
                              "Education", "Income")) +
  labs(x = "Regression Coefficients", y = NULL)

analysis <- d %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income, country,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income) %>%
  mutate(country = factor(country)) %>%
  filter(missing_trust == 0,
         missing_age == 0)
analysis <- within(analysis, country <- relevel(country, ref = "The Netherlands"))
m <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                 rile_selfplacement + factor(gov_performance) + 
                 factor(pid) + factor(gender) + age +
                 education + income + factor(country)+ factor(missing_wtac) +
                 factor(missing_rile_selfplacement) + factor(missing_pid) +
                 factor(missing_gender) + 
                 factor(missing_education) + factor(missing_income),
               data = analysis)
r3 <- ggcoefstats(
  x = tidy(m)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title = "Political Trust Predicted by Willingness to Compromise",
  subtitle = "List-Wise Deletion for Missing Values on Trust and Age",
  ggtheme = theme_minimal(),
  ggstatsplot.layer = F) + # note the order in which the labels are entered
  scale_y_discrete(labels = c("Willingness to Accept Compromise", 
                              "Not Politically Interested", 
                              "Left-Right Self-Placement",
                              "Satisfied with Government Performance",
                              "Partisan ID",
                              "Male",
                              "Age",
                              "Education", "Income")) +
  labs(x = "Regression Coefficients", y = NULL) 