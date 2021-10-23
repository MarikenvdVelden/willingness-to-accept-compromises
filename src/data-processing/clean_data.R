# Tidy Qualtrics Data

# Clean demographics
dem <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(matches("D\\d"), id = tic) %>%
  select(-matches("D6_7_TEXT")) %>%
  select(id, D4:D10)%>%
  drop_na()

pret <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(matches("PT\\d"), id = tic, S1:S2) %>%
  select(-matches("PT8_NPS_GROUP")) %>%
  select(id, PT1_1:PT4, PT5 = PT5_1, PT6:S2) %>%
  drop_na() 

treat <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(id = tic, issue, compromise, outcome, partner) %>%
  drop_na() 

dv <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(id = tic, matches("DV[123]"), matches("MC[123]")) %>%
  select(-matches("DV[123]_NPS_GROUP"), -MC3_9) %>%
  mutate(MC3_1 = if_else(MC3_1 == 1, "CDU/CSU", "NA"),
         MC3_2 = if_else(MC3_2 == 1, "SPD", "NA"),
         MC3_3 = if_else(MC3_3 == 1, "AfD", "NA"),
         MC3_4 = if_else(MC3_4 == 1, "FDP", "NA"),
         MC3_5 = if_else(MC3_5 == 1, "DIE LINKE", "NA"),
         MC3_6 = if_else(MC3_6 == 1, "BÜNDNIS 90/DIE GRÜNEN", "NA")) %>%
  unite(MC3, matches("MC3_\\d"), na.rm = TRUE, sep = ", ") %>%
  drop_na()

ht <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
        tic != "359766d9041e7d8c345c0c2838781095", 
        tic != "daabba41247f457b637205e066bbe39b",
        tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(id = tic, matches("HT\\d")) %>%
  select(-matches("HT3[abc]_NPS_GROUP")) %>%
  drop_na() 

post <- d %>%
  filter(Consent == 4, AC2_1 ==1, AC2_4 == 1,
         tic != "359766d9041e7d8c345c0c2838781095", 
         tic != "daabba41247f457b637205e066bbe39b",
         tic != "f6b4160f5ab5dee05b7e925d50e5414") %>%
  select(id = tic, matches("POST1[ab]_[123456]")) %>%
  unite(POST_1, matches("POST1[ab]_1"), na.rm = TRUE) %>%
  unite(POST_2, matches("POST1[ab]_2"), na.rm = TRUE) %>%
  unite(POST_3, matches("POST1[ab]_3"), na.rm = TRUE) %>%
  unite(POST_4, matches("POST1[ab]_4"), na.rm = TRUE) %>%
  unite(POST_5, matches("POST1[ab]_5"), na.rm = TRUE) %>%
  unite(POST_6, matches("POST1[ab]_6"), na.rm = TRUE) %>%
  mutate(POST_1 = as.numeric(POST_1),
         POST_2 = as.numeric(POST_2),
         POST_3 = as.numeric(POST_3),
         POST_4 = as.numeric(POST_4),
         POST_5 = as.numeric(POST_5),
         POST_6 = as.numeric(POST_6)) %>%
  drop_na()

d <- left_join(dem, pret, by = "id")
d <- left_join(d, treat, by = "id")
d <- left_join(d, dv, by = "id")
d <- left_join(d, ht, by = "id")
d <- left_join(d, post, by = "id") 
rm(dem, pret, treat, post, ht, dv)
