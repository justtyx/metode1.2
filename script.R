setwd("C:/Users/justi/OneDrive/Desktop/statskundskab/Metode 1/Skriveøvelse 2/r")
getwd()
#load packages
library(haven)
library(tidyverse)
library(scales)  
library(psych)
library(e1071)
#load data
cses6 <- read_dta("cses6/CSES6.dta")
# Filtrér til kun Danmark
cses_dk <- cses6 %>%
  filter(F1006_NAM == "Denmark")
# Klargør variabler
df <- cses_dk %>%
  select(
    interesse = F3001,           # Q1
    tv = F3002_1,                # Q2a
    radio = F3002_3,             # Q2b
    avis = F3002_4,              # Q2c
    #some = F3002_6_1,            # Q2d
    intern_effektivitet = F3003, #Q3
    stem2022 = F3010_LH,         # Q12a
    stem2019 = F3015_LH,         # Q18a
    uddannelse = F2003
  )

# Ryd op i irrelevante og ikke-besvarede værdier
df <- df %>%
  mutate(
    # Interesse: fjern 7 = ønsker ikke at svare og 8 = ved ikke
    interesse = na_if(interesse, 7),
    interesse = na_if(interesse, 8),
    intern_effektivitet = na_if(interesse, 7),
    intern_effektivitet = na_if(interesse, 8),
    
    
    # Mediebrug: fjern 97 og 98
    tv = na_if(tv, 97),
    tv = na_if(tv, 98),
    radio = na_if(radio, 97),
    radio = na_if(radio, 98),
    avis = na_if(avis, 97),
    avis = na_if(avis, 98),
    #some = na_if(some, 97),
    #some = na_if(some, 98),
    
    # Stemme: fjern 3 = ikke stemmeberettiget, 7 og 8
    stem2022 = na_if(stem2022, 3),
    stem2022 = na_if(stem2022, 7),
    stem2022 = na_if(stem2022, 8),
    stem2019 = na_if(stem2019, 3),
    stem2019 = na_if(stem2019, 7),
    stem2019 = na_if(stem2019, 8)
  ) %>%
  mutate(
    # Reverse kod stemmevariabler: 1 = stemt → 1, 2 = ikke stemt → 0
    stem2022_bin = case_when(
      stem2022 == 1 ~ 1,
      stem2022 == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    stem2019_bin = case_when(
      stem2019 == 1 ~ 1,
      stem2019 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Skaler variabler
df <- df %>%
  mutate(
    interesse_rev = 5 - interesse,
    interesse_scaled = scales::rescale(interesse_rev, to = c(0, 1)),
    intern_effektivitet_rev = 5 - intern_effektivitet,
    intern_effektivitet_scaled = scales::rescale(intern_effektivitet_rev, to = c(0, 1)),
    tv_scaled = scales::rescale(tv - 1, to = c(0, 1)),
    radio_scaled = scales::rescale(radio - 1, to = c(0, 1)),
    avis_scaled = scales::rescale(avis - 1, to = c(0, 1)),
    #some_scaled = scales::rescale(some - 1, to = c(0, 1))
  )

# Lav engagement-indeks
df <- df %>%
  rowwise() %>%
  mutate(
    engagement_rå = mean(c_across(c(
      interesse_scaled, tv_scaled, radio_scaled,
      avis_scaled, #some_scaled, 
      intern_effektivitet_scaled, stem2022_bin, stem2019_bin
    )), na.rm = TRUE),
    engagement_mangler = sum(is.na(c_across(c(
      interesse_scaled, tv_scaled, radio_scaled,
      avis_scaled, #some_scaled, 
      intern_effektivitet_scaled, stem2022_bin, stem2019_bin
    )))),
    engagement_index = if_else(engagement_mangler <= 1, engagement_rå, NA_real_)
  ) %>%
  ungroup()

# Fjern rækker med manglende data
df <- df %>%
  filter(!is.na(engagement_index), !is.na(uddannelse))

# Beregn typetal
getmode <- function(x) {
  uniq <- na.omit(unique(x))
  uniq[which.max(tabulate(match(x, uniq)))]
}
getmode(df$engagement_index)

# Beskrivende statistik
mean(df$engagement_index, na.rm = TRUE)
sd(df$engagement_index, na.rm = TRUE)

middelvaerdi <- mean(df$engagement_index, na.rm = TRUE)
spredning <- sd(df$engagement_index, na.rm = TRUE)

c(middelvaerdi - spredning, middelvaerdi + spredning)
c(middelvaerdi - 2 * spredning, middelvaerdi + 2 * spredning)
c(middelvaerdi - 3 * spredning, middelvaerdi + 3 * spredning)

# Skævhed og kurtosis
skew(df$engagement_index, na.rm = TRUE)
kurtosis(df$engagement_index, na.rm = TRUE)

# Cronbach's alpha
psych::alpha(df[, c(
  "interesse_scaled",
  "tv_scaled",
  "radio_scaled",
  "avis_scaled",
  #"some_scaled",
  "intern_effektivitet_scaled",
  "stem2022_bin",
  "stem2019_bin"
)])