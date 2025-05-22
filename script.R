setwd("C:/Users/justi/OneDrive/Desktop/statskundskab/Metode 1/Skriveøvelse 2/r")
getwd()
#load packages
library(haven)
library(tidyverse)
library(scales)  
library(psych)
library(dplyr)
library(tidyr)
library(e1071)
library(broom)
# Typetal funktion
getmode <- function(x) {
  uniq <- na.omit(unique(x))
  uniq[which.max(tabulate(match(x, uniq)))]
}
#load data
cses6 <- read_dta("cses6/CSES6.dta")
# ------------------------------------------------------
# Univariat
# ------------------------------------------------------
  # Filtrér til kun Danmark
  cses_dk <- cses6 %>%
    filter(F1006_NAM == "Denmark")
  # Klargør variabler
  tillid_dk <- cses_dk |>
    select(
      parlament = F3007_1,
      regering = F3007_2,
      retsvæsenet = F3007_3,
      partier = F3007_5,
      uddannelse = F2003,
      køn = F2002,
      alder = F2001_A
    )
  # Fjern 7 = ønsker ikke at svare og 6 = ved ikke
  tillid_dk  <- tillid_dk  |>
    mutate(
    across(c(parlament, regering, retsvæsenet, partier), ~ na_if(., 7)),
    across(c(parlament, regering, retsvæsenet, partier), ~ na_if(., 8))
  )
  # Reverse-kod
  tillid_dk  <- tillid_dk  |>
    mutate(
      parlament =  5 - parlament, 
      regering = 5- regering,
      retsvæsenet = 5- retsvæsenet,
      partier = 5 - partier
    )
  # Konstruer indeks: gennemsnit hvis højst én NA
  tillid_dk <- tillid_dk |>
    rowwise() |>
    mutate(
      tillid_rå = mean(c_across(c(parlament, regering, retsvæsenet, partier)), na.rm = TRUE),
      tillid_mangler = sum(is.na(c_across(c(parlament, regering, retsvæsenet, partier)))),
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
      "retsvæsenet",
      "partier"
    )])
  # Beregn gennemsnit og standardafvigelse for tillid
  middelvaerdi_tillid <- mean(tillid_dk$tillid, na.rm = TRUE)
  spredning_tillid <- sd(tillid_dk$tillid, na.rm = TRUE)
  # Skævhed
  skew(tillid_dk$tillid, na.rm = TRUE)
  # Kurtosis
  kurtosis(tillid_dk$tillid, na.rm = TRUE)
  #Histogram
  ggplot(tillid_dk, aes(x = tillid)) +
    geom_histogram(binwidth = 0.25, fill = "#901A1E", color = "black", boundary = 1) +
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
# ------------------------------------------------------
# Bivariat
# ------------------------------------------------------
  # Ryd op i manglende værdier for uddannelse
  tillid_dk <- tillid_dk %>%
    mutate(
      uddannelse = na_if(uddannelse, 97),
      uddannelse = na_if(uddannelse, 98),
      uddannelse = na_if(uddannelse, 10)
    ) %>%
    mutate(
      uddannelse_gruppe = factor(case_when(
        uddannelse %in% 1:2 ~ "Grundskole",
        uddannelse %in% 3:4 ~ "Gymnasial",
        uddannelse == 5    ~ "Erhvervsfaglig gymnasial",
        uddannelse == 6    ~ "Kort videregående",
        uddannelse == 7    ~ "Mellemlang videregående",
        uddannelse == 8    ~ "Lang videregående",
        uddannelse == 9    ~ "Forskeruddannelse",
        TRUE               ~ NA_character_
      ), levels = c(
        "Grundskole", "Gymnasial", "Erhvervsfaglig gymnasial",
        "Kort videregående", "Mellemlang videregående",
        "Lang videregående", "Forskeruddannelse"
      ))
    )
  #2.1 Sammenfatning af politisk tillid efter uddannelse
  opsummering_bivariat <- tillid_dk %>%
    group_by(uddannelse_gruppe) %>%
    summarise(
      antal             = n(),
      gennemsnit        = round(mean(tillid, na.rm = TRUE), 2),
      median            = median(tillid, na.rm = TRUE),
      standardafvigelse = round(sd(tillid, na.rm = TRUE), 2)
    )
  print(opsummering_bivariat)
  #Boxplot: politisk tillid efter uddannelsesniveau
  ggplot(tillid_dk, aes(x = uddannelse_gruppe, y = tillid)) +
    geom_boxplot(fill = "#901A1E", color = "black", outlier.shape = NA) +
    labs(
      title = "Politisk tillid fordelt på uddannelsesniveau (2022)",
      x     = "Uddannelsesniveau",
      y     = "Politisk tillid (1 = lav, 4 = høj)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title  = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title  = element_text(size = 12)
    ) +
    ylim(1, 4)
  
  #Kategorisér politisk tillid i tre niveauer
  tillid_dk <- tillid_dk %>%
    mutate(
      tillid_kategori = factor(case_when(
        tillid < 2               ~ "Lav",
        tillid >= 2 & tillid < 3 ~ "Middel",
        tillid >= 3              ~ "Høj",
        TRUE                     ~ NA_character_
      ), levels = c("Lav","Middel","Høj"))
    )
  #Krydstabel med procentfordeling inden for hver uddannelsesgruppe
  tabel_kryds <- tillid_dk %>%
    filter(!is.na(uddannelse_gruppe), !is.na(tillid_kategori)) %>%
    count(uddannelse_gruppe, tillid_kategori) %>%
    group_by(uddannelse_gruppe) %>%
    mutate(
      total   = sum(n),
      procent = round(100 * n / total, 1)
    ) %>%
    select(uddannelse_gruppe, tillid_kategori, procent) %>%
    pivot_wider(names_from = tillid_kategori, values_from = procent, values_fill = 0)
  #Tilføj samlet total-række
  total_raekke <- tillid_dk %>%
    filter(!is.na(tillid_kategori)) %>%
    count(tillid_kategori) %>%
    mutate(procent = round(100 * n / sum(n), 1)) %>%
    pivot_wider(names_from = tillid_kategori, values_from = procent) %>%
    mutate(uddannelse_gruppe = "Total") %>%
    select(uddannelse_gruppe, Lav, Middel, Høj)
  
  kryds_tabel_med_total <- bind_rows(tabel_kryds, total_raekke)
  print(kryds_tabel_med_total)
  #Chi-i-anden-test for uafhængighed mellem uddannelse og tillid
  resultat_chi2 <- table(tillid_dk$uddannelse_gruppe, tillid_dk$tillid_kategori) %>%
    chisq.test()
  print(resultat_chi2)
  #T-test: Grundskole vs. Forskeruddannelse
  resultat_ttest <- tillid_dk %>%
    filter(uddannelse_gruppe %in% c("Grundskole","Forskeruddannelse")) %>%
    t.test(tillid ~ uddannelse_gruppe, data = .)
  print(resultat_ttest)
# ------------------------------------------------------
# OLS
# ------------------------------------------------------
  # Ryd op i køn og alder
  tillid_dk <- tillid_dk %>%
    mutate(
      køn         = na_if(køn, 7),
      køn         = na_if(køn, 8),
      alder       = na_if(alder, 999),
      køn_faktor  = factor(køn, levels = c(1,0), labels = c("Kvinde","Mand")),
      alder_gruppe = factor(case_when(
        alder >= 18 & alder <= 29 ~ "18-29",
        alder >= 30 & alder <= 39 ~ "30-39",
        alder >= 40 & alder <= 49 ~ "40-49",
        alder >= 50 & alder <= 59 ~ "50-59",
        alder >= 60 & alder <= 69 ~ "60-69",
        alder >= 70              ~ "70+",
        TRUE                      ~ NA_character_
      ), levels = c("18-29","30-39","40-49","50-59","60-69","70+"))
    )
  #Opret liste af modeller
  modeller <- list(
    model1 = lm(tillid ~ uddannelse_gruppe,                         data = tillid_dk),
    model2 = lm(tillid ~ uddannelse_gruppe + køn_faktor,           data = tillid_dk),
    model3 = lm(tillid ~ uddannelse_gruppe + alder_gruppe,         data = tillid_dk),
    model4 = lm(tillid ~ uddannelse_gruppe + køn_faktor + alder_gruppe, data = tillid_dk)
  )
  #Oversigt over modeller med broom
  oversigt_modeller <- lapply(modeller, broom::tidy, conf.int = TRUE)
  oversigt_modeller
  #Endelig model med alle kovariater
  samlet_model <- lm(tillid ~ køn_faktor + alder_gruppe + uddannelse_gruppe, data = tillid_dk)
  print(broom::tidy(samlet_model, conf.int = TRUE))
  #Plot af koefficienter for model4
  diagram_data4 <- broom::tidy(modeller$model4, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = recode(term,
                    # Uddannelsesgrupper
                    "uddannelse_gruppeGymnasial"                = "Gymnasial",
                    "uddannelse_gruppeErhvervsfaglig gymnasial" = "Erhvervsfaglig gymnasial",
                    "uddannelse_gruppeKort videregående"        = "Kort videregående",
                    "uddannelse_gruppeMellemlang videregående"  = "Mellemlang videregående",
                    "uddannelse_gruppeLang videregående"        = "Lang videregående",
                    "uddannelse_gruppeForskeruddannelse"        = "Forskeruddannelse",
                    # Køn
                    "køn_faktorMand"                            = "Mand",
                    # Aldersgrupper
                    "alder_gruppe30-39"                         = "30–39 år",
                    "alder_gruppe40-49"                         = "40–49 år",
                    "alder_gruppe50-59"                         = "50–59 år",
                    "alder_gruppe60-69"                         = "60–69 år",
                    "alder_gruppe70+"                           = "70+ år"
      ),
      term = factor(term, levels = rev(c(
        # Gør rækkefølgen pæn i diagrammet
        "Forskeruddannelse",
        "Lang videregående",
        "Mellemlang videregående",
        "Kort videregående",
        "Erhvervsfaglig gymnasial",
        "Gymnasial",
        "Mand",
        "30–39 år",
        "40–49 år",
        "50–59 år",
        "60–69 år",
        "70+ år"
      )))
    )
  
  ggplot(diagram_data4, aes(x = term, y = estimate)) +
    geom_hline(
      yintercept = 0,
      linetype   = "dashed",
      color      = "#901A1E",
      linewidth  = 0.8
    ) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
    coord_flip() +
    labs(
      title = "OLS-regression på politisk tillid (model 4)",
      x     = "",
      y     = "Estimat (B) med 95% konfidensinterval"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  