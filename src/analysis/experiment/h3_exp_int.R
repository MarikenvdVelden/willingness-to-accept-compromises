## Calculate effects for interactions

# Intercept: value for trust WHEN no compromise & coalition talks stalled
# a: value for trust WHEN compromise & coalition talks stalled
# b: value for trust WHEN no compromise & coalition talks continue
# a:b:  value for trust WHEN  compromise & coalition talks continue 

## POOLED 
df <- d %>% 
  mutate(a = if_else(compromise=="yes", 1, 0),
         b = if_else(outcome == "negotiation", 1, 0),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))
m1 <- tidy(lm(DV1 ~ a*b +
          factor(S1) + S2 + factor(partner) +
          PT8 + PT1_1 + PT1_2 + PT3_2 +
          factor(issue) +
          factor(D4) + factor(D7) +
          factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

#a <- 1.40 + (-.313*0) + (-.161*1) + (0.119*0*1) #no compromise & coalition talks continue
#b  <- 1.40 + (-.313*0) + (-.161*0) + (0.119*0*0) #no compromise & coalition talks stalled
#a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 
#c <- 1.40 + (-.313*1) + (-.161*1) + (0.119*1*1) #compromise & coalition talks continue
#d <- 1.40 + (-.313*1) + (-.161*0) + (0.119*1*0) # compromise & coalition talks stalled
#c-d #more trust compromise & coalition talks stalled than compromise & coalition talks continue

a <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*0) + 
  (m1[m1$term=="b","estimate"]*1) + 
  (m1[m1$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue

b  <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*0) + 
  (m1[m1$term=="b","estimate"]*0) + 
  (m1[m1$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*1) + 
  (m1[m1$term=="b","estimate"]*1) + 
  (m1[m1$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*1) + 
  (m1[m1$term=="b","estimate"]*0) + 
  (m1[m1$term=="a:b","estimate"]*1*0) # compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
              term2 = c("coalition talks continue", "coalition talks stalled",
                        "coalition talks continue", "coalition talks stalled"),
              value = est,
              y = "DV: Trust",
              sig = "No Statistically Significant Difference") 

m2 <- tidy(lm(DV2 ~ a*b +
          factor(S1) + S2 + factor(partner) +
          PT8 + PT1_1 + PT1_2 + PT3_2 +
          factor(issue) +
          factor(D4) + factor(D7) +
          factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*0) + 
  (m2[m2$term=="b","estimate"]*1) + 
  (m2[m2$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue
b  <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*0) + 
  (m2[m2$term=="b","estimate"]*0) + 
  (m2[m2$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*1) + 
  (m2[m2$term=="b","estimate"]*1) + 
  (m2[m2$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*1) + 
  (m2[m2$term=="b","estimate"]*0) + 
  (m2[m2$term=="a:b","estimate"]*1*0)# compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>% 
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                            "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Credibility",
                  sig = "No Statistically Significant Difference") 
  )

m3 <- tidy(lm(DV2 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(issue) +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*0) + 
  (m3[m3$term=="b","estimate"]*1) + 
  (m3[m3$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue
b  <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*0) + 
  (m3[m3$term=="b","estimate"]*0) + 
  (m3[m3$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*1) + 
  (m3[m3$term=="b","estimate"]*1) + 
  (m3[m3$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*1) + 
  (m3[m3$term=="b","estimate"]*0) + 
  (m3[m3$term=="a:b","estimate"]*1*0)# compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>%
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                               "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Representation",
                  sig = "No Statistically Significant Difference")) %>% 
  mutate(issue = "Pooled Analysis")

# Tax 
df <- d %>% 
  filter(issue=="TopTax") %>% 
  mutate(a = if_else(compromise=="yes", 1, 0),
         b = if_else(outcome == "negotiation", 1, 0),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))
m1 <- tidy(lm(DV1 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*0) + 
  (m1[m1$term=="b","estimate"]*1) + 
  (m1[m1$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue

b  <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*0) + 
  (m1[m1$term=="b","estimate"]*0) + 
  (m1[m1$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*1) + 
  (m1[m1$term=="b","estimate"]*1) + 
  (m1[m1$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*1) + 
  (m1[m1$term=="b","estimate"]*0) + 
  (m1[m1$term=="a:b","estimate"]*1*0) # compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>% 
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
              term2 = c("coalition talks continue", "coalition talks stalled",
                        "coalition talks continue", "coalition talks stalled"),
              value = est,
              y = "DV: Trust",
              sig = "No Statistically Significant Difference",
              issue = "TopTax"))

m2 <- tidy(lm(DV2 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*0) + 
  (m2[m2$term=="b","estimate"]*1) + 
  (m2[m2$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue
b  <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*0) + 
  (m2[m2$term=="b","estimate"]*0) + 
  (m2[m2$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*1) + 
  (m2[m2$term=="b","estimate"]*1) + 
  (m2[m2$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*1) + 
  (m2[m2$term=="b","estimate"]*0) + 
  (m2[m2$term=="a:b","estimate"]*1*0)# compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>% 
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                            "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Credibility",
                  sig = "No Statistically Significant Difference",
                  issue = "TopTax") 
  )

m3 <- tidy(lm(DV2 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*0) + 
  (m3[m3$term=="b","estimate"]*1) + 
  (m3[m3$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue
b  <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*0) + 
  (m3[m3$term=="b","estimate"]*0) + 
  (m3[m3$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*1) + 
  (m3[m3$term=="b","estimate"]*1) + 
  (m3[m3$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*1) + 
  (m3[m3$term=="b","estimate"]*0) + 
  (m3[m3$term=="a:b","estimate"]*1*0)# compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>%
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                            "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Representation",
                  sig = "No Statistically Significant Difference",
                  issue = "TopTax"))


# SpeedLimit 
df <- d %>% 
  filter(issue=="SpeedLimit") %>% 
  mutate(a = if_else(compromise=="yes", 1, 0),
         b = if_else(outcome == "negotiation", 1, 0),
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1))
m1 <- tidy(lm(DV1 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*0) + 
  (m1[m1$term=="b","estimate"]*1) + 
  (m1[m1$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue

b  <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*0) + 
  (m1[m1$term=="b","estimate"]*0) + 
  (m1[m1$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*1) + 
  (m1[m1$term=="b","estimate"]*1) + 
  (m1[m1$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m1[m1$term=="(Intercept)","estimate"] + 
  (m1[m1$term=="a","estimate"]*1) + 
  (m1[m1$term=="b","estimate"]*0) + 
  (m1[m1$term=="a:b","estimate"]*1*0) # compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>% 
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                            "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Trust",
                  sig = "No Statistically Significant Difference",
                  issue = "SpeedLimit"))

m2 <- tidy(lm(DV2 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*0) + 
  (m2[m2$term=="b","estimate"]*1) + 
  (m2[m2$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue
b  <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*0) + 
  (m2[m2$term=="b","estimate"]*0) + 
  (m2[m2$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*1) + 
  (m2[m2$term=="b","estimate"]*1) + 
  (m2[m2$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m2[m2$term=="(Intercept)","estimate"] + 
  (m2[m2$term=="a","estimate"]*1) + 
  (m2[m2$term=="b","estimate"]*0) + 
  (m2[m2$term=="a:b","estimate"]*1*0)# compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>% 
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                            "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Credibility",
                  sig = "No Statistically Significant Difference",
                  issue = "SpeedLimit") 
  )

m3 <- tidy(lm(DV2 ~ a*b +
                factor(S1) + S2 + factor(partner) +
                PT8 + PT1_1 + PT1_2 + PT3_2 +
                factor(D4) + factor(D7) +
                factor(D9) + factor(D10), df)) %>% 
  filter(term %in% c("(Intercept)","a", "b", "a:b"))

a <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*0) + 
  (m3[m3$term=="b","estimate"]*1) + 
  (m3[m3$term=="a:b","estimate"]*0*1) #no compromise & coalition talks continue
b  <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*0) + 
  (m3[m3$term=="b","estimate"]*0) + 
  (m3[m3$term=="a:b","estimate"]*0*0) #no compromise & coalition talks stalled
a-b # more trust no compromise & coalition talks stalled than no compromise & coalition talks continue 

c <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*1) + 
  (m3[m3$term=="b","estimate"]*1) + 
  (m3[m3$term=="a:b","estimate"]*1*1) #compromise & coalition talks continue
e <- m3[m3$term=="(Intercept)","estimate"] + 
  (m3[m3$term=="a","estimate"]*1) + 
  (m3[m3$term=="b","estimate"]*0) + 
  (m3[m3$term=="a:b","estimate"]*1*0)# compromise & coalition talks stalled
c-e #more trust compromise & coalition talks stalled than compromise & coalition talks continue
est <- rbind(a,b,c,e)

tmp <- tmp %>%
  add_case(tibble(term = c("No compromise", "No compromise", "Compromise","Compromise"),
                  term2 = c("coalition talks continue", "coalition talks stalled",
                            "coalition talks continue", "coalition talks stalled"),
                  value = est,
                  y = "DV: Representation",
                  sig = "No Statistically Significant Difference",
                  issue = "SpeedLimit")) %>% 
  mutate(y = factor(y, 
                    levels = c("DV: Trust", "DV: Credibility",
                               "DV: Representation")))
   

p2_int <- tmp %>% 
  ggplot(aes(x = term, y = value$estimate, 
             color = term2,
             group = term2)) +
  geom_point() +
  geom_line(linetype = "dotted", linewidth = .3) +
  theme_ipsum() +
  labs(x = "",
       y = "Interaction Effects") +
  facet_grid(issue~y, scales = "free") +
  scale_color_manual(values = fig_cols) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position="bottom",
        legend.title = element_blank())
