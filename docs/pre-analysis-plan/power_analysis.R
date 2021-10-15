# Power Analysis
rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(scales)
library(patchwork)

#H1 + H2: pooled over parties
N <- 100
assignment_prob <- 0.5 # 1 out of 2: steadfast vs. capitulate
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(2000,6000,1000), treatment_effect=seq(0.1,0.2,0.1))

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis1 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis1$diagnosands_df
diagnosis1  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis for H1 and H2")

#H3: pooled over parties
N <- 100
assignment_prob <- 0.25 # 1 out of 4 conditions: (no) compromise with(out) a government formed
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(2000,6000,1000), treatment_effect=seq(0.1,0.2,0.1))

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis2 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis2$diagnosands_df
diagnosis2  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis for H3")

p1 <- diagnosis1 %>% add_case(diagnosis2) %>%
  ggplot(aes(x=N, y=power.onetailed, group=factor(treatment_effect), colour=factor(treatment_effect))) +
  geom_line() +
  geom_hline(yintercept=0.95, linetype="dashed") +
  geom_hline(yintercept=0.8, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 4000,linetype="dotdash", color = "darkgrey") +
  ylim(c(0,1)) +
  labs(x = "Number of Respondents", y = "Power: One-Tailed",
       title = "Issue Specific Analysis",
       subtitle = "Various Levels of Treatment Effect") +
  scale_y_continuous(labels=percent) +
  theme_minimal() +
  facet_grid(.~id) +
  scale_color_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

#H1: pooled over parties & issues
N <- 100
assignment_prob <- 0.5 # 1 out of 2: steadfast vs. capitulate
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(6000,10000,2000), treatment_effect=seq(0.1,0.2,0.1))

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis1 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis1$diagnosands_df
diagnosis1  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis for H1 and H2")

#H2 + H3: pooled over parties
N <- 100
assignment_prob <- 0.25 # 1 out of 4 conditions: (no) compromise with(out) a government formed
treatment_effect <- 0.1

population <- declare_population(N = N, u = rnorm(N))
potential_outcomes <- declare_potential_outcomes(Y_Z_0 = u, Y_Z_1 = Y_Z_0 + treatment_effect)
estimand <- declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0))
assignment <- declare_assignment(prob = assignment_prob)
reveal_Y <- declare_reveal()
estimator <- declare_estimator(Y ~ Z, estimand = estimand, term="Z", model=lm)
two_arm_design <- population + potential_outcomes + estimand + 
  assignment + reveal_Y + estimator
designs <- redesign(design=two_arm_design, N=seq(6000,10000,2000), treatment_effect=seq(0.1,0.2,0.1))

alpha <- .05
my_diagnosands <- declare_diagnosands(power.onetailed=mean(p.value<alpha), keep_defaults=TRUE)
diagnosis2 <- diagnose_designs(designs,diagnosands=my_diagnosands)
diagnosis <- diagnosis2$diagnosands_df
diagnosis2  <- diagnosis %>% 
  select(N, treatment_effect, power.onetailed) %>%
  mutate(id = "Power-Analysis for H3")

p2 <- diagnosis1 %>% add_case(diagnosis2) %>%
  ggplot(aes(x=N, y=power.onetailed, group=factor(treatment_effect), colour=factor(treatment_effect))) +
  geom_line() +
  geom_hline(yintercept=0.95, linetype="dashed") +
  geom_hline(yintercept=0.8, linetype="dashed", color = "grey") +
  geom_vline(xintercept = 8000,linetype="dotdash", color = "darkgrey") +
  ylim(c(0,1)) +
  labs(x = "Number of Respondents", y = "Power: One-Tailed",
       title = "Issue-Pooled Analysis",
       subtitle = "Various Levels of Treatment Effect") +
  scale_y_continuous(labels=percent) +
  theme_minimal() +
  facet_grid(.~id) +
  scale_color_viridis_d() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

p1 + p2
ggsave("docs/pre-analysis-plan/power_analysis-pooled.png", width=8, height=5, dpi=900)
