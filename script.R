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
cses_engagement_dk <- cses_dk |>
  select(
    tv = F3002_1,                # Q2a
    radio = F3002_3,             # Q2b
    avis = F3002_4,              # Q2c
    some = F3002_6_1,            # Q2d
    uddannelse = F2003
  )

# Fjern 7/97 = ønsker ikke at svare og 8/98 = ved ikke
cses_engagement_dk  <- cses_engagement_dk  |>
  mutate(
    across(c(tv, radio, avis, some), ~ na_if(., 97)),
    across(c(tv, radio, avis, some), ~ na_if(., 98)),
  )

# Konstruer indeks: gennemsnit hvis højst én NA
cses_engagement_dk <- cses_engagement_dk |>
  rowwise() |>
  mutate(
    engagement_rå = mean(c_across(c(tv, radio, avis, some)), na.rm = TRUE),
    engagement_mangler = sum(is.na(c_across(c(tv, radio, avis, some)))),
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
  "tv",
  "radio",
  "avis",
  "some"
)])
