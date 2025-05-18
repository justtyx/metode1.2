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
tillid_dk <- cses_dk |>
  select(
parlament = F3007_1,
regering = F3007_2,
domstol = F3007_3,
partier = F3007_5,
    uddannelse = F2003
  )
# Fjern 98 = ønsker ikke at svare og 99 = ved ikke
tillid_dk  <- tillid_dk  |>
  mutate(
  across(c(parlament, regering, domstol, partier), ~ na_if(., 7)),
  across(c(parlament, regering, domstol, partier), ~ na_if(., 8))
)
# Reverse-kod
tillid_dk  <- tillid_dk  |>
  mutate(
    parlament =  5 - parlament, 
    regering = 5- regering,
    domstol = 5- domstol,
    partier = 5 - partier
  )
# Konstruer indeks: gennemsnit hvis højst én NA
tillid_dk <- tillid_dk |>
  rowwise() |>
  mutate(
    tillid_rå = mean(c_across(c(parlament, regering, domstol, partier)), na.rm = TRUE),
    tillid_mangler = sum(is.na(c_across(c(parlament, regering, domstol, partier)))),
    tillid = ifelse(tillid_mangler <= 1, tillid_rå, NA)
  ) |>
  ungroup()
  #opsummering
  summary(tillid_dk$tillid)
  #typetal
  typetal_tillid <- getmode(tillid_dk$tillid)
  print(typetal_tillid)
  # Cronbach's alpha
  psych::alpha(tillid_dk[, c(
    "parlament",
    "regering",
    "domstol",
    "partier"
  )])
  # Beregn gennemsnit og standardafvigelse for tillid
  middelvaerdi_tillid <- mean(tillid_dk$tillid, na.rm = TRUE)
  spredning_tillid <- sd(tillid_dk$tillid, na.rm = TRUE)
  # Beregn intervaller for ±1, ±2 og ±3 standardafvigelser
  interval_1sd_tillid <- c(middelvaerdi_tillid - spredning_tillid, middelvaerdi_tillid + spredning_tillid)   # Ca. 68 %
  interval_2sd_tillid <- c(middelvaerdi_tillid - 2 * spredning_tillid, middelvaerdi_tillid + 2 * spredning_tillid)  # Ca. 95 %
  interval_3sd_tillid <- c(middelvaerdi_tillid - 3 * spredning_tillid, middelvaerdi_tillid + 3 * spredning_tillid)  # Ca. 99,7 %
  # Udskriv intervaller
  interval_1sd_tillid
  interval_2sd_tillid
  interval_3sd_tillid
  # Skævhed
  skew(tillid_dk$tillid, na.rm = TRUE)
  # Kurtosis
  kurtosis(tillid_dk$tillid, na.rm = TRUE)
  #Histogram
  ggplot(tillid_dk, aes(x = tillid)) +
    geom_histogram(binwidth = 0.25, fill = "#3B82F6", color = "black", boundary = 1) +
    scale_x_continuous(breaks = 1:4, limits = c(1, 4)) +
    labs(
      title = "Fordeling af politisk tillid (2022)",
      x = "Politisk tillid (1 = lav, 5 = høj)",
      y = "Antal respondenter"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
      axis.title = element_text(size = 12),
      panel.grid.major = element_line(color = "#E5E7EB"),
      panel.grid.minor = element_blank()
    ) +
    ylim(0, 400)
