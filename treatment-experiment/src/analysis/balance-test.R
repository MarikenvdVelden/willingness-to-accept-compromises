covs <- d %>%
  select(treatment, D1:D5) %>% 
  drop_na()

balanced <-bal.tab(treatment ~ factor(D1) +  factor(D2) + D3 + 
                     factor(D4) + D5,
                   data = covs,
                   thresholds = c(m = 0.05))[[1]] 
df <- balanced %>%
  mutate(variable = c("Male", "Age: 18 - 29", "Age: 30 - 39", "Age: 40 - 49",
                      "Age: 50 - 59", "Age: 60 - 74", "Age: 75 or older",
                      "Education", "Vote Recall: AfD", "Vote Recall: Bündnis 90/Die Grünen",
                      "Vote Recall: CDU/CSU", "Vote Recall: Die Linke",
                      "Vote Recall: FDP", "Vote Recall: Other party",
                      "Vote Recall: SPD", "Digital literacy"),
         variable = factor(variable,
                           levels = c("Male", "Age: 18 - 29", "Age: 30 - 39", "Age: 40 - 49",
                                      "Age: 50 - 59", "Age: 60 - 74", "Age: 75 or older",
                                      "Education", "Vote Recall: AfD", "Vote Recall: Bündnis 90/Die Grünen",
                                      "Vote Recall: CDU/CSU", "Vote Recall: Die Linke",
                                      "Vote Recall: FDP", "Vote Recall: Other party",
                                      "Vote Recall: SPD", "Digital literacy")),
         difference = Diff.Un,2) %>%
  select(variable, difference) %>%
  mutate(type = if_else(difference <= -.05, "below",
                        if_else(difference >= .05, "below", "above"))) %>%
  ggplot(aes(x = variable, y = difference, color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("above"=fig_cols[3],
                                "below"=fig_cols[2])) +
  
  theme_bw() +
  labs(x="", y= "Standardized Mean Differences") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none",) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  geom_hline(yintercept = -0.05, linetype = "dashed") +
  coord_flip()

rm(covs, balanced)
