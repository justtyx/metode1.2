setwd("C:/Users/justi/OneDrive/Desktop/statskundskab/Metode 1/Skriveøvelse 2/r")
getwd()
#load packages
library(haven)
library(tidyverse)
library(scales)  
library(psych)
library(e1071)
# Typetal funktion
getmode <- function(x) {
  uniq <- na.omit(unique(x))
  uniq[which.max(tabulate(match(x, uniq)))]
}
#load data
cses6 <- read_dta("cses6/CSES6.dta")
# Filtrér til kun Danmark
cses_dk <- cses6 %>%
  filter(F1006_NAM == "Denmark")
# Klargør variabler
cses_engagement_dk <- cses_dk %>%
  select(
    interesse = F3001,           # Q1
    avis = F3002_4,              # Q2c
    intern_effektivitet = F3003, #Q3
    stem2022 = F3010_LH,         # Q12a
    uddannelse = F2003
  )

# Fjern 7/97 = ønsker ikke at svare og 8/98 = ved ikke
cses_engagement_dk  <- cses_engagement_dk  |>
  mutate(
    across(c(interesse, intern_effektivitet), ~ na_if(., 7)),
    across(c(interesse, intern_effektivitet), ~ na_if(., 8)),
    across(c(avis), ~ na_if(., 97)),
    across(c(avis), ~ na_if(., 98)),
    stem2022 = na_if(stem2022, 3),
    stem2022 = na_if(stem2022, 7),
    stem2022 = na_if(stem2022, 8)
  )

# Reverse-kod Q1 & Q3
cses_engagement_dk  <- cses_engagement_dk  |>
  mutate(
    interesse_rev = 5 - interesse,
    intern_effektivitet_rev = 6 - intern_effektivitet,
    stem2022_rev = 3 - stem2022
  )

# Skaler alle til 1-4 skala
cses_engagement_dk <- cses_engagement_dk |>
  mutate(
    interesse_rev_scaled = scales::rescale(interesse_rev, to = c(0, 1)),
    intern_effektivitet_rev_scaled = scales::rescale(intern_effektivitet_rev, to = c(0, 1)),
    avis_scaled = scales::rescale(as.numeric(avis), to = c(0, 1)),
    stem2022_rev_scaled = scales::rescale(as.numeric(stem2022_rev), to = c(0, 1))
  )

# Konstruer indeks: gennemsnit hvis højst én NA
cses_engagement_dk <- cses_engagement_dk |>
  rowwise() |>
  mutate(
    engagement_rå = mean(c_across(c(interesse_rev_scaled, intern_effektivitet_rev_scaled, avis_scaled,  stem2022_rev_scaled)), na.rm = TRUE),
    engagement_mangler = sum(is.na(c_across(c(interesse_rev_scaled, intern_effektivitet_rev_scaled, avis_scaled,  stem2022_rev_scaled)))),
    engagement_index = ifelse(engagement_mangler <= 1, engagement_rå, NA)
  ) |>
  ungroup()
#opsummering
summary(cses_engagement_dk$engagement_index)
#typetal
typetal_engagement <- getmode(cses_engagement_dk$engagement_index)
print(typetal_engagement)
# Cronbach's alpha
psych::alpha(cses_engagement_dk[, c(
  "interesse_rev_scaled",
  "avis_scaled",
  "intern_effektivitet_rev_scaled",
  "stem2022_rev_scaled"
)])
