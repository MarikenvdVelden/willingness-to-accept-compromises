# Tidy Qualtrics Data

# Clean demographics
pret <- d %>%
  filter(Consent == 4) %>%
  select(matches("D\\d"), id = ResponseId) %>%
  select(-matches("D4_7_TEXT")) %>%
  mutate(D1 = recode(D1,
                     `1` = "Male",
                     `2` = "Female",
                     .default = "NA"),
         D2 = recode(D2,
                     `2` = "18-29",
                     `3` = "30-39",
                     `4` = "40-49",
                     `5` = "50-59",
                     `6` = "60-74",
                     `7` = "75 or older"),
         D3 = ifelse(D3==4, 0, D3),
         D4 = recode(D4,
                     `1` = "CDU/CSU",
                     `2` = "SPD",
                     `3` = "AfD",
                     `4` = "FDP",
                     `5` = "Die Linke",
                     `6` = "Bündnis 90/Die Grünen",
                     `7` = "Other party",
                     .default = "NA"),
         D5_1 = D5_1 - 1,
         D5_2 = D5_2 - 1,
         D5_3 = D5_3 - 1,
         D5_4 = D5_4 - 1,
         D5_5 = D5_5 - 1,
         D5_6 = D5_6 - 1) %>%
  add_index(D5, D5_1:D5_6, type = "sum") %>% 
  select(id, D1:D4,D5) 
  
time <- d %>% 
  filter(Consent == 4) %>% 
  unite(time, c(`Q15_Page Submit`, `Q21_Page Submit`), na.rm = TRUE) %>% 
  mutate(time = as.numeric(time)) %>% 
  select(id = ResponseId, time)
  
treat <- d %>%  filter(Consent == 4) %>% 
  select(id = ResponseId, treatment = treat) %>%
  drop_na()

post <- d %>%
  filter(Consent == 4) %>%
  mutate(MC1 = ifelse(MC1 == 2,1,0),
         MC2 = ifelse(MC2 ==2, 1, 0),
         AC1_1 = replace_na(AC1_1, 0),
         AC1_4 = replace_na(AC1_4, 0),
         MC = ifelse(MC1 ==1 & MC2 ==1,1,0),
         AC = ifelse(AC1_1 ==1 & AC1_4 ==1,1,0),) %>% 
  select(id = ResponseId, MC, AC)  
  
d <- left_join(pret, time, by = "id")
d <- left_join(d, treat, by = "id")
d <- left_join(d, post, by = "id") 

d <- d %>% 
  mutate(D1 = ifelse(D1=="NA", "Male", D1),
         D1 = replace_na(D1, "Male"),
         D2 = ifelse(D2=="NA", "30-39", D2),
         D2 = replace_na(D2, "30-39"),
         D3 = replace_na(D3, round(mean(D3, na.rm=T),0)),
         D4 = ifelse(D4=="NA", "Bündnis 90/Die Grünen", D4),
         D4 = replace_na(D4, "Bündnis 90/Die Grünen"),
         time = replace_na(time, round(mean(time, na.rm=T),0)),
         MC = replace_na(MC, round(mean(MC, na.rm=T),0)))

rm(pret, time, treat, post)
