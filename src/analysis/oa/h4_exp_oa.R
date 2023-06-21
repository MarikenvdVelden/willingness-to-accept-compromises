# H4
issues <- unique(d$issue)
#principledness 2
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = HT1,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    t4 <- regression_ht_oa(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht_oa(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    t4 <- t4 %>% add_case(tmp) %>%
      mutate(type = "Principledness (2)")
  }
}

t4 <- t4 %>%
  filter(y != "DV3") %>%
  mutate(x = recode(x,
                    `compromise` = "Party Position: Steadfast",
                    `outcome` = "Outcome: Coalition Talks Continued",
                    `b` = "Principledness (2)",
                    `factor(S1)FDP` = "In-Party: FDP",
                    `factor(S1)Greens` = "In-Party: Greens",
                    `factor(S1)SPD` = "In-Party: SPD",
                    `S2` = "PID strength for In-Party",
                    `factor(partner)die Grünen` = "Negotiation Partner: Greens",
                    `factor(partner)FDP` = "Negotiation Partner: FDP",
                    `factor(partner)SPD` = "Negotiation Partner: SPD",
                    `PT8` = "Ideological Position",
                    `PT1_1` = "Issue Preference: Speed Limit",
                    `PT1_2` = "Issue Preference: TopTax",
                    `PT3_2` = "Issue Importance: TopTax",
                    `factor(D4)Countryside` = "Urbanization: Countryside",
                    `factor(D4)Middlesized City` = "Urbanization: Middlesized City",
                    `factor(D4)NA` = "Urbanization: Unknown",
                    `factor(D4)Rural Village` = "Urbanization: Rural Village",
                    `factor(D4)Suburb` = "Urbanization: Suburb",
                    `factor(D7)1` = "Employment Status: Full-time employed",
                    `factor(D7)2` = "Employment Status: Part-time employed",
                    `factor(D7)3` = "Employment Status: Marginally employed",
                    `factor(D7)4` = "Employment Status: In School",
                    `factor(D7)5` = "Employment Status: Studing",
                    `factor(D7)6` = "Employment Status: Re-schooling",
                    `factor(D7)7` = "Employment Status: Currently unemployed",
                    `factor(D7)8` = "Employment Status: Currently Reduced Hourse of Work",
                    `factor(D7)9` = "Employment Status: Volunteer",
                    `factor(D7)10` = "Employment Status: Parental Leave",
                    `factor(D7)11` = "Employment Status: Housewife/Househusband or else",
                    `factor(D9)Bavaria` = "Federal State: Bavaria",
                    `factor(D9)Berlin` = "Federal State: Berlin",
                    `factor(D9)Brandenburg` = "Federal State: Brandenburg",
                    `factor(D9)Bremen` = "Federal State: Bremen",
                    `factor(D9)Hamburg` = "Federal State: Hamburg",
                    `factor(D9)Hessen` = "Federal State: Hessen",
                    `factor(D9)Lower Saxony` = "Federal State: Lower Saxony",
                    `factor(D9)Mecklenburg-Western Pomerania` = "Federal State: Mecklenburg-Western Pomerania",
                    `factor(D9)NA` = "Federal State: Unknown",
                    `factor(D9)North Rhine-Westphalia` = "Federal State: North Rhine-Westphalia",
                    `factor(D9)Rhineland-Palatinate` = "Federal State: Rhineland-Palatinate",
                    `factor(D9)Saarland` = "Federal State: Saarland",
                    `factor(D9)Saxony` = "Federal State: Saxony",
                    `factor(D9)Saxony-Anholt` = "Federal State: Saxony-Anholt",
                    `factor(D9)Schleswig-Holstein` = "Federal State: Schleswig-Holstein",
                    `factor(D9)Thuringia` = "Federal State: Thuringia",
                    `factor(D10)Bavaria` = "Federal State Born: Bavaria",
                    `factor(D10)Berlin` = "Federal State Born: Berlin",
                    `factor(D10)Brandenburg` = "Federal State Born: Brandenburg",
                    `factor(D10)Bremen` = "Federal State Born: Bremen",
                    `factor(D10)Hamburg` = "Federal State Born: Hamburg",
                    `factor(D10)Hessen` = "Federal State Born: Hessen",
                    `factor(D10)Lower Saxony` = "Federal State Born: Lower Saxony",
                    `factor(D10)Mecklenburg-Western Pomerania` = "Federal State Born: Mecklenburg-Western Pomerania",
                    `factor(D10)NA` = "Federal State Born: Unknown",
                    `factor(D10)North Rhine-Westphalia` = "Federal State Born: North Rhine-Westphalia",
                    `factor(D10)Rhineland-Palatinate` = "Federal State Born: Rhineland-Palatinate",
                    `factor(D10)Not born in Germany` = "Not Born in Germany",
                    `factor(D10)Saarland` = "Federal State Born: Saarland",
                    `factor(D10)Saxony` = "Federal State Born: Saxony",
                    `factor(D10)Saxony-Anholt` = "Federal State Born: Saxony-Anholt",
                    `factor(D10)Schleswig-Holstein` = "Federal State Born: Schleswig-Holstein",
                    `factor(D10)Thuringia` = "Federal State Born: Thuringia",
                    `compromise:b` = "Interaction: Outcome * Principledness (2)"),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility"),
         type = "Principledness (2)") %>%
  filter(estimate != is.na(estimate))

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT1,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
t4p <- pooled_regression_ht_oa(df, compromise, outcome, b, issue) %>% 
  filter(y != "DV3") %>%
  mutate(x = recode(x,
                    `compromise` = "Party Position: Steadfast",
                    `outcome` = "Outcome: Coalition Talks Continued",
                    `b` = "Principledness (2)",
                    `factor(S1)FDP` = "In-Party: FDP",
                    `factor(S1)Greens` = "In-Party: Greens",
                    `factor(S1)SPD` = "In-Party: SPD",
                    `S2` = "PID strength for In-Party",
                    `factor(partner)die Grünen` = "Negotiation Partner: Greens",
                    `factor(partner)FDP` = "Negotiation Partner: FDP",
                    `factor(partner)SPD` = "Negotiation Partner: SPD",
                    `PT8` = "Ideological Position",
                    `PT1_1` = "Issue Preference: Speed Limit",
                    `PT1_2` = "Issue Preference: TopTax",
                    `PT3_2` = "Issue Importance: TopTax",
                    `factor(D4)Countryside` = "Urbanization: Countryside",
                    `factor(D4)Middlesized City` = "Urbanization: Middlesized City",
                    `factor(D4)NA` = "Urbanization: Unknown",
                    `factor(D4)Rural Village` = "Urbanization: Rural Village",
                    `factor(D4)Suburb` = "Urbanization: Suburb",
                    `factor(D7)1` = "Employment Status: Full-time employed",
                    `factor(D7)2` = "Employment Status: Part-time employed",
                    `factor(D7)3` = "Employment Status: Marginally employed",
                    `factor(D7)4` = "Employment Status: In School",
                    `factor(D7)5` = "Employment Status: Studing",
                    `factor(D7)6` = "Employment Status: Re-schooling",
                    `factor(D7)7` = "Employment Status: Currently unemployed",
                    `factor(D7)8` = "Employment Status: Currently Reduced Hourse of Work",
                    `factor(D7)9` = "Employment Status: Volunteer",
                    `factor(D7)10` = "Employment Status: Parental Leave",
                    `factor(D7)11` = "Employment Status: Housewife/Househusband or else",
                    `factor(D9)Bavaria` = "Federal State: Bavaria",
                    `factor(D9)Berlin` = "Federal State: Berlin",
                    `factor(D9)Brandenburg` = "Federal State: Brandenburg",
                    `factor(D9)Bremen` = "Federal State: Bremen",
                    `factor(D9)Hamburg` = "Federal State: Hamburg",
                    `factor(D9)Hessen` = "Federal State: Hessen",
                    `factor(D9)Lower Saxony` = "Federal State: Lower Saxony",
                    `factor(D9)Mecklenburg-Western Pomerania` = "Federal State: Mecklenburg-Western Pomerania",
                    `factor(D9)NA` = "Federal State: Unknown",
                    `factor(D9)North Rhine-Westphalia` = "Federal State: North Rhine-Westphalia",
                    `factor(D9)Rhineland-Palatinate` = "Federal State: Rhineland-Palatinate",
                    `factor(D9)Saarland` = "Federal State: Saarland",
                    `factor(D9)Saxony` = "Federal State: Saxony",
                    `factor(D9)Saxony-Anholt` = "Federal State: Saxony-Anholt",
                    `factor(D9)Schleswig-Holstein` = "Federal State: Schleswig-Holstein",
                    `factor(D9)Thuringia` = "Federal State: Thuringia",
                    `factor(D10)Bavaria` = "Federal State Born: Bavaria",
                    `factor(D10)Berlin` = "Federal State Born: Berlin",
                    `factor(D10)Brandenburg` = "Federal State Born: Brandenburg",
                    `factor(D10)Bremen` = "Federal State Born: Bremen",
                    `factor(D10)Hamburg` = "Federal State Born: Hamburg",
                    `factor(D10)Hessen` = "Federal State Born: Hessen",
                    `factor(D10)Lower Saxony` = "Federal State Born: Lower Saxony",
                    `factor(D10)Mecklenburg-Western Pomerania` = "Federal State Born: Mecklenburg-Western Pomerania",
                    `factor(D10)NA` = "Federal State Born: Unknown",
                    `factor(D10)North Rhine-Westphalia` = "Federal State Born: North Rhine-Westphalia",
                    `factor(D10)Rhineland-Palatinate` = "Federal State Born: Rhineland-Palatinate",
                    `factor(D10)Not born in Germany` = "Not Born in Germany",
                    `factor(D10)Saarland` = "Federal State Born: Saarland",
                    `factor(D10)Saxony` = "Federal State Born: Saxony",
                    `factor(D10)Saxony-Anholt` = "Federal State Born: Saxony-Anholt",
                    `factor(D10)Schleswig-Holstein` = "Federal State Born: Schleswig-Holstein",
                    `factor(D10)Thuringia` = "Federal State Born: Thuringia",
                    `compromise:b` = "Interaction: Outcome * Principledness (2)"),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility"),
         issue = "Pooled Analysis",
         type = "Principledness (2)") %>%
  filter(estimate != is.na(estimate))
         

t4 <- t4 %>% 
  add_case(t4p) 

t4a <- t4 %>% 
  filter(y == "DV: Trust" & issue == "TopTax")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4b <- t4 %>% 
  filter(y == "DV: Trust" & issue == "SpeedLimit")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4c <- t4 %>% 
  filter(y == "DV: Trust" & issue == "Pooled Analysis")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4d <- t4 %>% 
  filter(y != "DV: Trust" & issue == "TopTax")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4e <- t4 %>% 
  filter(y != "DV: Trust" & issue == "SpeedLimit")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4f <- t4 %>% 
  filter(y != "DV: Trust" & issue == "Pooled Analysis")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)


#principledness1
for(i in 1:length(issues)){
  df <- d %>% 
    mutate(compromise = if_else(compromise=="yes", 0, 1),
           outcome = if_else(outcome == "negotiation", 0, 1),
           b = HT2,
           partner = recode(partner, 
                            "CDU" = 1,
                            "die Grünen"= 0,
                            "FDP" = 0,
                            "SPD" = 1)) %>%
    filter(issue == issues[i])
  if(i==1){
    t4 <- regression_ht_oa(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
  }  
  else{
    tmp <- regression_ht_oa(df, compromise, outcome, b) %>%
      mutate(issue = issues[i])
    t4 <- t4 %>% add_case(tmp) %>%
      mutate(type = "Principledness (1)")
  }
}

t4 <- t4 %>%
  filter(y != "DV3") %>%
  mutate(x = recode(x,
                    `compromise` = "Party Position: Steadfast",
                    `outcome` = "Outcome: Coalition Talks Continued",
                    `b` = "Principledness (1)",
                    `factor(S1)FDP` = "In-Party: FDP",
                    `factor(S1)Greens` = "In-Party: Greens",
                    `factor(S1)SPD` = "In-Party: SPD",
                    `S2` = "PID strength for In-Party",
                    `factor(partner)die Grünen` = "Negotiation Partner: Greens",
                    `factor(partner)FDP` = "Negotiation Partner: FDP",
                    `factor(partner)SPD` = "Negotiation Partner: SPD",
                    `PT8` = "Ideological Position",
                    `PT1_1` = "Issue Preference: Speed Limit",
                    `PT1_2` = "Issue Preference: TopTax",
                    `PT3_2` = "Issue Importance: TopTax",
                    `factor(D4)Countryside` = "Urbanization: Countryside",
                    `factor(D4)Middlesized City` = "Urbanization: Middlesized City",
                    `factor(D4)NA` = "Urbanization: Unknown",
                    `factor(D4)Rural Village` = "Urbanization: Rural Village",
                    `factor(D4)Suburb` = "Urbanization: Suburb",
                    `factor(D7)1` = "Employment Status: Full-time employed",
                    `factor(D7)2` = "Employment Status: Part-time employed",
                    `factor(D7)3` = "Employment Status: Marginally employed",
                    `factor(D7)4` = "Employment Status: In School",
                    `factor(D7)5` = "Employment Status: Studing",
                    `factor(D7)6` = "Employment Status: Re-schooling",
                    `factor(D7)7` = "Employment Status: Currently unemployed",
                    `factor(D7)8` = "Employment Status: Currently Reduced Hourse of Work",
                    `factor(D7)9` = "Employment Status: Volunteer",
                    `factor(D7)10` = "Employment Status: Parental Leave",
                    `factor(D7)11` = "Employment Status: Housewife/Househusband or else",
                    `factor(D9)Bavaria` = "Federal State: Bavaria",
                    `factor(D9)Berlin` = "Federal State: Berlin",
                    `factor(D9)Brandenburg` = "Federal State: Brandenburg",
                    `factor(D9)Bremen` = "Federal State: Bremen",
                    `factor(D9)Hamburg` = "Federal State: Hamburg",
                    `factor(D9)Hessen` = "Federal State: Hessen",
                    `factor(D9)Lower Saxony` = "Federal State: Lower Saxony",
                    `factor(D9)Mecklenburg-Western Pomerania` = "Federal State: Mecklenburg-Western Pomerania",
                    `factor(D9)NA` = "Federal State: Unknown",
                    `factor(D9)North Rhine-Westphalia` = "Federal State: North Rhine-Westphalia",
                    `factor(D9)Rhineland-Palatinate` = "Federal State: Rhineland-Palatinate",
                    `factor(D9)Saarland` = "Federal State: Saarland",
                    `factor(D9)Saxony` = "Federal State: Saxony",
                    `factor(D9)Saxony-Anholt` = "Federal State: Saxony-Anholt",
                    `factor(D9)Schleswig-Holstein` = "Federal State: Schleswig-Holstein",
                    `factor(D9)Thuringia` = "Federal State: Thuringia",
                    `factor(D10)Bavaria` = "Federal State Born: Bavaria",
                    `factor(D10)Berlin` = "Federal State Born: Berlin",
                    `factor(D10)Brandenburg` = "Federal State Born: Brandenburg",
                    `factor(D10)Bremen` = "Federal State Born: Bremen",
                    `factor(D10)Hamburg` = "Federal State Born: Hamburg",
                    `factor(D10)Hessen` = "Federal State Born: Hessen",
                    `factor(D10)Lower Saxony` = "Federal State Born: Lower Saxony",
                    `factor(D10)Mecklenburg-Western Pomerania` = "Federal State Born: Mecklenburg-Western Pomerania",
                    `factor(D10)NA` = "Federal State Born: Unknown",
                    `factor(D10)North Rhine-Westphalia` = "Federal State Born: North Rhine-Westphalia",
                    `factor(D10)Rhineland-Palatinate` = "Federal State Born: Rhineland-Palatinate",
                    `factor(D10)Not born in Germany` = "Not Born in Germany",
                    `factor(D10)Saarland` = "Federal State Born: Saarland",
                    `factor(D10)Saxony` = "Federal State Born: Saxony",
                    `factor(D10)Saxony-Anholt` = "Federal State Born: Saxony-Anholt",
                    `factor(D10)Schleswig-Holstein` = "Federal State Born: Schleswig-Holstein",
                    `factor(D10)Thuringia` = "Federal State Born: Thuringia",
                    `compromise:b` = "Interaction: Outcome * Principledness (1)"),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility"),
         type = "Principledness (1)") %>%
  filter(estimate != is.na(estimate))

df <- d %>% 
  mutate(compromise = if_else(compromise=="yes", 0, 1),
         outcome = if_else(outcome == "negotiation", 0, 1),
         b = HT2,
         partner = recode(partner, 
                          "CDU" = 1,
                          "die Grünen"= 0,
                          "FDP" = 0,
                          "SPD" = 1)) 
t4p <- pooled_regression_ht_oa(df, compromise, outcome, b, issue) %>% 
  filter(y != "DV3") %>%
  mutate(x = recode(x,
                    `compromise` = "Party Position: Steadfast",
                    `outcome` = "Outcome: Coalition Talks Continued",
                    `b` = "Principledness (1)",
                    `factor(S1)FDP` = "In-Party: FDP",
                    `factor(S1)Greens` = "In-Party: Greens",
                    `factor(S1)SPD` = "In-Party: SPD",
                    `S2` = "PID strength for In-Party",
                    `factor(partner)die Grünen` = "Negotiation Partner: Greens",
                    `factor(partner)FDP` = "Negotiation Partner: FDP",
                    `factor(partner)SPD` = "Negotiation Partner: SPD",
                    `PT8` = "Ideological Position",
                    `PT1_1` = "Issue Preference: Speed Limit",
                    `PT1_2` = "Issue Preference: TopTax",
                    `PT3_2` = "Issue Importance: TopTax",
                    `factor(D4)Countryside` = "Urbanization: Countryside",
                    `factor(D4)Middlesized City` = "Urbanization: Middlesized City",
                    `factor(D4)NA` = "Urbanization: Unknown",
                    `factor(D4)Rural Village` = "Urbanization: Rural Village",
                    `factor(D4)Suburb` = "Urbanization: Suburb",
                    `factor(D7)1` = "Employment Status: Full-time employed",
                    `factor(D7)2` = "Employment Status: Part-time employed",
                    `factor(D7)3` = "Employment Status: Marginally employed",
                    `factor(D7)4` = "Employment Status: In School",
                    `factor(D7)5` = "Employment Status: Studing",
                    `factor(D7)6` = "Employment Status: Re-schooling",
                    `factor(D7)7` = "Employment Status: Currently unemployed",
                    `factor(D7)8` = "Employment Status: Currently Reduced Hourse of Work",
                    `factor(D7)9` = "Employment Status: Volunteer",
                    `factor(D7)10` = "Employment Status: Parental Leave",
                    `factor(D7)11` = "Employment Status: Housewife/Househusband or else",
                    `factor(D9)Bavaria` = "Federal State: Bavaria",
                    `factor(D9)Berlin` = "Federal State: Berlin",
                    `factor(D9)Brandenburg` = "Federal State: Brandenburg",
                    `factor(D9)Bremen` = "Federal State: Bremen",
                    `factor(D9)Hamburg` = "Federal State: Hamburg",
                    `factor(D9)Hessen` = "Federal State: Hessen",
                    `factor(D9)Lower Saxony` = "Federal State: Lower Saxony",
                    `factor(D9)Mecklenburg-Western Pomerania` = "Federal State: Mecklenburg-Western Pomerania",
                    `factor(D9)NA` = "Federal State: Unknown",
                    `factor(D9)North Rhine-Westphalia` = "Federal State: North Rhine-Westphalia",
                    `factor(D9)Rhineland-Palatinate` = "Federal State: Rhineland-Palatinate",
                    `factor(D9)Saarland` = "Federal State: Saarland",
                    `factor(D9)Saxony` = "Federal State: Saxony",
                    `factor(D9)Saxony-Anholt` = "Federal State: Saxony-Anholt",
                    `factor(D9)Schleswig-Holstein` = "Federal State: Schleswig-Holstein",
                    `factor(D9)Thuringia` = "Federal State: Thuringia",
                    `factor(D10)Bavaria` = "Federal State Born: Bavaria",
                    `factor(D10)Berlin` = "Federal State Born: Berlin",
                    `factor(D10)Brandenburg` = "Federal State Born: Brandenburg",
                    `factor(D10)Bremen` = "Federal State Born: Bremen",
                    `factor(D10)Hamburg` = "Federal State Born: Hamburg",
                    `factor(D10)Hessen` = "Federal State Born: Hessen",
                    `factor(D10)Lower Saxony` = "Federal State Born: Lower Saxony",
                    `factor(D10)Mecklenburg-Western Pomerania` = "Federal State Born: Mecklenburg-Western Pomerania",
                    `factor(D10)NA` = "Federal State Born: Unknown",
                    `factor(D10)North Rhine-Westphalia` = "Federal State Born: North Rhine-Westphalia",
                    `factor(D10)Rhineland-Palatinate` = "Federal State Born: Rhineland-Palatinate",
                    `factor(D10)Not born in Germany` = "Not Born in Germany",
                    `factor(D10)Saarland` = "Federal State Born: Saarland",
                    `factor(D10)Saxony` = "Federal State Born: Saxony",
                    `factor(D10)Saxony-Anholt` = "Federal State Born: Saxony-Anholt",
                    `factor(D10)Schleswig-Holstein` = "Federal State Born: Schleswig-Holstein",
                    `factor(D10)Thuringia` = "Federal State Born: Thuringia",
                    `compromise:b` = "Interaction: Outcome * Principledness (1)"),
         y = recode(y,
                    `DV1` = "DV: Trust",
                    `DV2` = "DV: Credibility"),
         issue = "Pooled Analysis",
         type = "Principledness (1)") %>%
  filter(estimate != is.na(estimate))

t4 <- t4 %>% 
  add_case(t4p) 

t4g <- t4 %>% 
  filter(y == "DV: Trust" & issue == "TopTax")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4h <- t4 %>% 
  filter(y == "DV: Trust" & issue == "SpeedLimit")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4i <- t4 %>% 
  filter(y == "DV: Trust" & issue == "Pooled Analysis")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4j <- t4 %>% 
  filter(y != "DV: Trust" & issue == "TopTax")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4k <- t4 %>% 
  filter(y != "DV: Trust" & issue == "SpeedLimit")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)

t4l <- t4 %>% 
  filter(y != "DV: Trust" & issue == "Pooled Analysis")  %>% 
  dplyr::select(`Independent Variables` = x, 
                Estimate = estimate, 
                `Std. Error` = std.error, 
                `P-value` = p.value)
