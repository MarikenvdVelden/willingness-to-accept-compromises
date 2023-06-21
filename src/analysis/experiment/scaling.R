HT1a <- d %>% select(matches("HT1a_\\d")) %>% drop_na()
scale <- tibble(alpha = round(ltm::cronbach.alpha(HT1a)[1]$alpha,2),
                variable = "Idealism")
HT1b <- d %>% select(matches("HT1b_\\d")) %>% drop_na()
scale <- scale %>% add_row(alpha = round(ltm::cronbach.alpha(HT1b)[1]$alpha,2),
                           variable = "Relativism")
HT3 <- d %>% select(matches("HT3[abc]")) %>% drop_na()
scale <- scale %>% add_row(alpha = round(ltm::cronbach.alpha(HT3)[1]$alpha,2),
                           variable = "Mutual Trust")
POST <- d %>% select(matches("POST_\\d")) %>% drop_na()
scale <- scale %>% add_row(alpha = round(ltm::cronbach.alpha(POST)[1]$alpha,2),
                           variable = "Populist Attitudes") %>%
  select(Variable = variable, `Cronbach's alpha` = alpha)

rm(HT1a, HT1b, HT3, POST)