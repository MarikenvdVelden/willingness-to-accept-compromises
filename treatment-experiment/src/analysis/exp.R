d <- d %>% 
  mutate(insta = ifelse(treatment == "insta", 1, 0 ),
         D2 = recode(D2,
                     `18-29` = 0,
                     `30-39` = 1,
                     `40-49` = 2,
                     `50-59` = 3,
                     `60-74` = 4,
                     `75 or older` = 5))

h1a <- lm(MC ~ factor(insta)* D2 + D5, d) 
h1a <-  summary(margins(h1a, variables = "insta", at = list(D2 = 0:5))) %>%
  mutate(b = D2,
         dv = "DV1: Manipulation Checks",
         #h = ifelse(term == "factor(treatment)insta", "Hypothesis", "Controls"),
         lower = AME - (1.56 * SE),
         upper = AME + (1.56 * SE),
         type = "Marginal Effects of Age Groups") %>%
  select(AME, upper, lower, dv, b, type)

h2a <- lm(time ~ factor(insta) * D2 + D5, d)
h2a <-  summary(margins(h2a, variables = "insta", at = list(D2 = 0:5))) %>%
  mutate(b = D2,
         dv = "DV2: Time",
         lower = AME - (1.56 * SE),
         upper = AME + (1.56 * SE),
         type = "Marginal Effects of Age Groups") %>%
  select(AME, upper, lower, dv, b, type)

h3a <- lm(AC ~ factor(insta)* D2 + D5, d)
h3a <- summary(margins(h3a, variables = "insta", at = list(D2 = 0:5))) %>%
  mutate(b = D2,
         dv = "DV3: Attention Check",
         lower = AME - (1.56 * SE),
         upper = AME + (1.56 * SE),
         type = "Marginal Effects of Age Groups") %>%
  select(AME, upper, lower, dv, b, type)

he <- h1a %>% 
  add_case(h2a) %>% 
  add_case(h3a) 


h1b <- lm(MC ~ factor(insta)* D5 + D2, d) 
h1b <-  summary(margins(h1b, variables = "insta", at = list(D5 = 0:18))) %>%
  mutate(b = D5,
         dv = "DV1: Manipulation Checks",
         #h = ifelse(term == "factor(treatment)insta", "Hypothesis", "Controls"),
         lower = AME - (1.56 * SE),
         upper = AME + (1.56 * SE),
         type = "Marginal Effects of Digital Literacy") %>%
  select(AME, upper, lower, dv, b, type)

h2b <- lm(time ~ factor(insta) * D5 + D2, d)
h2b <-  summary(margins(h2b, variables = "insta", at = list(D5 = 0:18))) %>%
  mutate(b = D5, 
         dv = "DV2: Time",
         lower = AME - (1.56 * SE),
         upper = AME + (1.56 * SE),
         type = "Marginal Effects of Digital Literacy") %>%
  select(AME, upper, lower, dv, b, type)

h3b <- lm(AC ~ factor(insta)* D5 + D2, d)
h3b <-  summary(margins(h3b, variables = "insta", at = list(D5 = 0:18))) %>%
  mutate(b = D5, 
         dv = "DV3: Attention Check",
         lower = AME - (1.56 * SE),
         upper = AME + (1.56 * SE),
         type = "Marginal Effects of Digital Literacy") %>%
  select(AME, upper, lower, dv, b, type)

he <- he %>% 
  add_case(h1b) %>% 
  add_case(h2b) %>% 
  add_case(h3b)  %>%   
  ggplot(aes(x = b, y = AME, 
             ymin = lower, ymax = upper)) +
  geom_line(color = fig_cols[1]) +
  geom_ribbon(fill = fig_cols[1], alpha = .2) +
  facet_grid(dv ~ type, scales = "free") +
  theme_ipsum() +
  labs(x = "", y = "") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .5, color = "darkgray")


