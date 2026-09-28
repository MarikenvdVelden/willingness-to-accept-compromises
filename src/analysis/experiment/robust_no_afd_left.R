# R&R RC4: Figures 2 and 3 excluding respondents who voted AfD or Die Linke in 2021

# --- setup (same as the top of OnlineAppendix.Rmd) ---
source(here::here("src/lib/functions.R"))
load(here("data/intermediate/cleaned_experiment.RData"))
source(here("src/analysis/experiment/data-for-analyses.R"))

# --- drop AfD and Die Linke voters (D6 = vote choice 2021) ---
d_full <- d
d <- d_full %>% filter(!D6 %in% c("AfD", "Left"))
print(table(d_full$D6))                         # vote distribution, full sample
cat("Excluded:", nrow(d_full) - nrow(d), " Remaining N:", nrow(d), "\n")

# --- Figure 2 (H1 & H2) ---
source(here("src/analysis/experiment/h1_exp.R"))
p1_nal <- p1 + labs(subtitle = "Excluding AfD and Die Linke voters (2021 vote)")
ggsave(here("report/figures/robust-no-afd-left-1.png"), p1_nal,
       width = 10, height = 5, dpi = 300, bg = "white")
print(h1 %>% filter(x %in% c("a", "b"), y != "DV3") %>%
        select(issue, y, x, estimate, std.error, p.value))

# --- Figure 3 (H3) ---
source(here("src/analysis/experiment/h3_exp.R"))
p2_nal <- p2 + labs(subtitle = "Excluding AfD and Die Linke voters (2021 vote)")
ggsave(here("report/figures/robust-no-afd-left-2.png"), p2_nal,
       width = 10, height = 5, dpi = 300, bg = "white")
print(h3 %>% add_case(h3p) %>% filter(y != "DV: Representation") %>%
        select(issue, y, x, estimate, std.error, p.value) %>%
        mutate(p.value = round(p.value, 3)), n = Inf)

d <- d_full; rm(d_full)