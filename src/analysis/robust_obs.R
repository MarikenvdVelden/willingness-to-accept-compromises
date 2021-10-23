tmp <- d %>%
  filter(country == "Austria") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
aut <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income,
                 data = tmp)
p1 <- ggcoefstats(
  x = tidy(aut)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title =  "Austria",
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

tmp <- d %>%
  filter(country == "France") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
fr <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                  rile_selfplacement + factor(gov_performance) + 
                  factor(pid) + factor(gender) + age +
                  education + income,
                data = tmp)
p2 <- ggcoefstats(
  x = tidy(fr)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title =  "France",
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

tmp <- d %>%
  filter(country == "Germany") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
de  <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income,
                 data = tmp)
p3 <- ggcoefstats(
  x = tidy(de)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title = "Germany",
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

tmp <- d %>%
  filter(country == "Italy") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
it  <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income + missing_rile_selfplacement,
                 data = tmp)
p4 <- ggcoefstats(
  x = tidy(it)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title = "Italy",
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

tmp <- d %>%
  filter(country == "The Netherlands") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
nl  <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income + missing_rile_selfplacement +
                   missing_trust + missing_wtac + missing_education +
                   missing_gov_performance + missing_pid,
                 data = tmp)
p5 <- ggcoefstats(
  x = tidy(nl)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title = "The Netherlands",
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

tmp <- d %>%
  filter(country == "Great Britain") %>%
  dplyr::select(trust, wtac, political_interest, rile_selfplacement,
                gov_performance, pid, gender, age, education, income,
                missing_trust, missing_wtac, missing_rile_selfplacement,
                missing_gov_performance, missing_pid, missing_gender, 
                missing_age, missing_education, missing_income)
gb  <- stats::lm(formula = trust ~ wtac + factor(political_interest) +
                   rile_selfplacement + factor(gov_performance) + 
                   factor(pid) + factor(gender) + age +
                   education + income + missing_rile_selfplacement +
                   missing_trust + missing_wtac + missing_education +
                   missing_gov_performance + missing_pid +
                   missing_income + missing_gender + missing_age,
                 data = tmp)
p6 <- ggcoefstats(
  x = tidy(gb)[2:10,],
  point.args = list(color = "seagreen", size = 3, shape = 15, alpha = .75),
  vline.args = list(size = 1, color = "darkgrey", linetype = "dotdash"),
  statistic = "t",
  stats.label.color = "seagreen",
  only.significant = TRUE,
  package = "palettesForR",
  palette = "Inkscape",
  title = "Great Britain",
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
