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
    tv = F3002_1,                # Q2a
    radio = F3002_3,             # Q2b
    avis = F3002_4,              # Q2c
    intern_effektivitet = F3003, #Q3
    uddannelse = F2003
  )

# Fjern 7/97 = ønsker ikke at svare og 8/98 = ved ikke
cses_engagement_dk  <- cses_engagement_dk  |>
  mutate(
    across(c(interesse, intern_effektivitet), ~ na_if(., 7)),
    across(c(interesse, intern_effektivitet), ~ na_if(., 8)),
    across(c(interesse, tv, avis, intern_effektivitet), ~ na_if(., 97)),
    across(c(interesse, tv, avis, intern_effektivitet), ~ na_if(., 98)),
  )

# Reverse-kod Q1 & Q3
cses_engagement_dk  <- cses_engagement_dk  |>
  mutate(
    interesse_rev = 5 - interesse,
    intern_effektivitet_rev = 6 - intern_effektivitet
  )

# Skaler alle til 1-4 skala
cses_engagement_dk <- cses_engagement_dk |>
  mutate(
    interesse_rev_scaled = scales::rescale(interesse_rev, to = c(1, 4)),
    intern_effektivitet_rev_scaled = scales::rescale(intern_effektivitet_rev, to = c(1, 4)),
    tv_scaled = scales::rescale(as.numeric(tv), to = c(1, 4)),
    radio_scaled = scales::rescale(as.numeric(radio), to = c(1, 4)),
    avis_scaled = scales::rescale(as.numeric(avis), to = c(1, 4))
  )

# Konstruer indeks: gennemsnit hvis højst én NA
cses_engagement_dk <- cses_engagement_dk |>
  rowwise() |>
  mutate(
    engagement_rå = mean(c_across(c(interesse_rev_scaled, intern_effektivitet_rev_scaled, tv_scaled, radio_scaled, avis_scaled)), na.rm = TRUE),
    engagement_mangler = sum(is.na(c_across(c(interesse_rev_scaled, intern_effektivitet_rev_scaled, tv_scaled, radio_scaled, avis_scaled)))),
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
  "tv_scaled",
  "radio_scaled",
  "avis_scaled",
  "intern_effektivitet_rev_scaled"
)])
