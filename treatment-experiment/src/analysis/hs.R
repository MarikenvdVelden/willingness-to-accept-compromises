d <- d %>% 
  mutate(treatment = factor(treatment,
                            levels = c("text", "insta")),
         D2 = recode(D2,
                     `18-29` = 0,
                     `30-39` = 1,
                     `40-49` = 2,
                     `50-59` = 3,
                     `60-74` = 4,
                     `75 or older` = 5))

h1 <- tidy(lm(MC ~ factor(treatment) + D2 + D5, d)) %>% 
  mutate(dv = "DV1: Manipulation Checks",
         h = ifelse(term == "factor(treatment)insta", "Hypothesis", "Controls"))
h2 <- tidy(lm(time ~ factor(treatment) + D2 + D5, d))  %>% 
  mutate(dv = "DV2: Time",
         h = ifelse(term == "factor(treatment)insta", "Hypothesis", "Controls"))
h3 <- tidy(lm(AC ~ factor(treatment) + D2 + D5, d))  %>% 
  mutate(dv = "DV3: Attention Check",
         h = ifelse(term == "factor(treatment)insta", "Hypothesis", "Controls"))


h <- h1 %>% 
  add_case(h2) %>% 
  add_case(h3) %>% 
  mutate(lower = estimate - 1.65*`std.error`,
         upper = estimate + 1.65*`std.error`,
         term = recode(term,
                       `(Intercept)` = "Intercept",
                       `factor(treatment)insta` = "Treatment: Instragram Post",
                       `D2` = "Age",
                       `D5` = "Digital Literacy"),
         term = factor(term,
                       levels = c("Age",
                                  "Digital Literacy",
                                  "Treatment: Instragram Post",
                                  "Intercept")),
         sign = ifelse(lower<0 & upper <0, 1, 0),
         sign = ifelse(lower>0 & upper>0,1,sign),
         sign = as.factor(sign)) %>% 
  ggplot(aes(y = term, x = estimate, 
             xmin = lower, xmax = upper,
             color = sign)) +
  geom_point() +
  geom_errorbar(width = 0) +
  facet_grid(. ~ dv, scales = "free") +
  labs(x = "Estimated Effect on DV",
       y = "") +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "gray75") +
  theme_ipsum() +
  scale_color_manual(values = c("#9DDAF5CC", fig_cols[1])) +
  theme(legend.position = "none")
