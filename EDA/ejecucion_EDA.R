### EDA ejecución municipalidades

library(tidyverse)
library(readxl)
library(sandwich)
library(plm)
library(lmtest)
library(broom)
library(clipr)
library(skimr)
theme_set(theme_minimal())

ejecucion_df <- read_xlsx("data/02_intermediate/ejecucion_municipalidad.xlsx")

ejecucion_df <- ejecucion_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015),
           region = str_c(str_sub(UBIGEO, 1, 2), "0000"),
           provincia = str_c(str_sub(UBIGEO, 1, 4), "00")
           )

# 

ejecucion_df %>% 
    skimr::skim() %>% 
    write_clip()

ejecucion_df %>% 
    select(year, Avance) %>% 
    group_by(year) %>% 
    skim() %>% 
    write_clip()


ejecucion_df %>% 
    pivot_longer(-c(Municipalidad, year, EY, UBIGEO, region, provincia)) %>% 
    ggplot(
        aes(x = year, y = value, group = Municipalidad)
    ) +
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    facet_wrap(~name, ncol = 1) +
    theme_bw()

ejecucion_df %>% 
    group_by(Municipalidad, EY, UBIGEO) %>% 
    arrange(Municipalidad, UBIGEO, EY, year) %>% 
    mutate(
        perc_advance_yoy_avance = c(0, diff(Avance)),
        perc_advance_yoy_avance_canon = c(0, diff(Avance_CANON))
    ) %>% 
    select(-c(Avance, Avance_CANON)) %>% 
    pivot_longer(-c(Municipalidad, year, EY, UBIGEO)) %>% 
    ggplot(
        aes(x = year, y = value, group = Municipalidad)
    ) +
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    facet_wrap(~name + EY, ncol = 1) +
    theme_bw()

ejecucion_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    ggplot(
        aes(x = year, group = year, y = Avance)
    ) +
    geom_violin()

ejecucion_df %>% 
    rowwise() %>% 
    mutate(Avance = Avance/100) %>% 
    ggplot(
        aes(x = year, group = year, y = Avance)
    ) +
    geom_boxplot(notch = TRUE) +
    scale_x_continuous(breaks = 2006:2019) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Avance", x = "Año")

ejecucion_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment
    ) %>% 
    coeftest() %>% 
    tidy(vcov = vcovHAC) %>% 
    write_clip()

ejecucion_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment + EY
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance_CANON ~ year + treatment + time_since_treatment + EY
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region
    ) %>%
    coeftest(vcov = sandwich::vcovHC) %>% 
    tidy() %>% 
    write_clip()
 
ejecucion_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region + provincia
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region + provincia
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_panel = plm::pdata.frame(ejecucion_df, index = c("Municipalidad", "year"))
ejecucion_df %>% 
    plm(
        data = .,
        formula = Avance ~ treatment + time_since_treatment,
        model = "within", effect = "individual"
    ) %>% 
    coeftest(vcov = vcovHC) %>% 
    tidy() %>% 
    write_clip()

ejecucion_df %>% 
    group_by(year) %>% 
    summarise(
        mean_avance = mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = mean_avance)
    ) + 
    geom_point() +
    geom_line() +
    ylim(c(0, 100))

ejecucion_df %>% 
    group_by(year) %>% 
    summarise(
        mean_avance = mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = mean_avance)
    ) + 
    geom_point() +
    geom_line() +
    ylim(c(0, 100))

ejecucion_df %>% 
    group_by(Municipalidad) %>%
    mutate(
        individual_difference = Avance - mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = Municipalidad)
    ) + 
    geom_point(alpha = 0.1) +
    geom_line(alpha = 0.1) +
    ylim(c(-100, 100))

ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = Municipalidad)
    ) + 
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    ylim(c(-100, 100)) +
    labs(x = "Año", y = "Diferencia",
         subtitle = "Diferencia entre la ejecución del año y la ejecución promedio pre-intervención")

ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    group_by(year < 2015) %>% 
    summarise(
        mean_mean = mean(individual_difference, na.rm = TRUE)
    )

ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = year)
    ) + 
    geom_jitter(alpha = 0.1, pch = ".") +
    geom_violin(alpha = 0.05) +
    ylim(c(-100, 100)) +
    labs(x = "Año", y = "Diferencia",
         subtitle = "Diferencia entre la ejecución del año y la ejecución promedio pre-intervención") +
    scale_x_continuous(breaks = 2007:2019) +
    geom_vline(xintercept = 2014.5, color = "red")
#    geom_hline(yintercept = 0, color = "black", lty = "dashed")
#    geom_hline(yintercept = -1.26, color = "black", lty = "dashed") 


ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        individual_difference
    ) %>% 
    write_clip()

top_bad <- ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        individual_difference
    ) %>% 
    head(10) %>% 
    pull(Municipalidad)
    
ejecucion_df %>% 
    filter(
        Municipalidad %in% top_bad
    ) %>% 
    select(Municipalidad, year, Avance) %>% 
    pivot_wider(names_from = year, values_from = Avance) %>% 
    clipr::write_clip()

ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        desc(individual_difference)
    ) %>% head(10) %>% 
    write_clip()

top_good <- ejecucion_df %>% 
    left_join(ejecucion_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        desc(individual_difference)
    ) %>% 
    head(10) %>% 
    pull(Municipalidad)

ejecucion_df %>% 
    filter(
        Municipalidad %in% top_good
    ) %>% 
    select(Municipalidad, region, year, Avance) %>% 
    pivot_wider(names_from = year, values_from = Avance) %>% 
    clipr::write_clip()

#### REMOVE MISSING DATA

missings_df <- ejecucion_df %>% 
    mutate(total_years = n_distinct(year)) %>% 
    group_by(Municipalidad) %>% 
    summarise(
        available_years = n(),
        total_years = max(total_years),
        perc_na = 1 - n()/total_years,
        filtered_from_sample = perc_na > 0.5) %>% 
    arrange(desc(perc_na))

###

filtered_df <- ejecucion_df %>% 
    filter(
        Municipalidad %in% (missings_df %>% 
            filter(!filtered_from_sample) %>% 
            pull(Municipalidad))
    )


filtered_df <- filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015),
           region = str_c(str_sub(UBIGEO, 1, 2), "0000"),
           provincia = str_c(str_sub(UBIGEO, 1, 4), "00")
    )

# 

filtered_df %>% 
    select(year, Avance) %>% 
    group_by(year) %>% 
    skim() %>% 
    write_clip()

filtered_df %>% 
    pivot_longer(-c(Municipalidad, year, EY, UBIGEO, region, provincia)) %>% 
    ggplot(
        aes(x = year, y = value, group = Municipalidad)
    ) +
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    facet_wrap(~name, ncol = 1, scales = "free_y") +
    theme_bw()

filtered_df %>% 
    group_by(Municipalidad, EY, UBIGEO) %>% 
    arrange(Municipalidad, UBIGEO, EY, year) %>% 
    mutate(
        perc_advance_yoy_avance = c(0, diff(Avance)),
        perc_advance_yoy_avance_canon = c(0, diff(Avance_CANON))
    ) %>% 
    select(-c(Avance, Avance_CANON)) %>% 
    pivot_longer(-c(Municipalidad, year, EY, UBIGEO, region, provincia)) %>% 
    ggplot(
        aes(x = year, y = value, group = Municipalidad)
    ) +
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    facet_wrap(~name + EY, ncol = 1) +
    theme_bw()

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    ggplot(
        aes(x = year, group = year, y = Avance)
    ) +
    geom_violin()

filtered_df %>% 
    rowwise() %>% 
    mutate(Avance = Avance/100) %>% 
    ggplot(
        aes(x = year, group = year, y = Avance)
    ) +
    geom_boxplot(notch = TRUE) +
    scale_x_continuous(breaks = 2006:2019) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Avance", x = "Año")

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment
    ) %>% 
    coeftest() %>% 
    tidy(vcov = vcovHAC) %>% 
    write_clip()

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment + EY
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance_CANON ~ year + treatment + time_since_treatment + EY
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region
    ) %>%
    coeftest(vcov = sandwich::vcovHC) %>% 
    tidy() %>% 
    write_clip()


filtered_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region + provincia
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region + provincia
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_panel = plm::pdata.frame(filtered_df, index = c("Municipalidad", "year"))
filtered_df %>% 
    plm(
        data = .,
        formula = Avance ~ treatment + time_since_treatment,
        model = "within", effect = "individual"
    ) %>% 
    coeftest(vcov = vcovHC) %>% 
    tidy() %>% 
    write_clip()

filtered_df %>% 
    group_by(year) %>% 
    summarise(
        mean_avance = mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = mean_avance)
    ) + 
    geom_point() +
    geom_line() +
    ylim(c(0, 100))

filtered_df %>% 
    group_by(year) %>% 
    summarise(
        mean_avance = mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = mean_avance)
    ) + 
    geom_point() +
    geom_line() +
    ylim(c(0, 100))

filtered_df %>% 
    group_by(Municipalidad) %>%
    mutate(
        individual_difference = Avance - mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = Municipalidad)
    ) + 
    geom_point(alpha = 0.1) +
    geom_line(alpha = 0.1) +
    ylim(c(-100, 100))

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = Municipalidad)
    ) + 
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    ylim(c(-100, 100)) +
    labs(x = "Año", y = "Diferencia",
         subtitle = "Diferencia entre la ejecución del año y la ejecución promedio pre-intervención")

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    group_by(year < 2015) %>% 
    summarise(
        mean_mean = mean(individual_difference, na.rm = TRUE)
    )

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = year)
    ) + 
    geom_jitter(alpha = 0.1, pch = ".") +
    geom_violin(alpha = 0.05) +
    ylim(c(-100, 100)) +
    labs(x = "Año", y = "Diferencia",
         subtitle = "Diferencia entre la ejecución del año y la ejecución promedio pre-intervención") +
    scale_x_continuous(breaks = 2007:2019) +
    geom_vline(xintercept = 2014.5, color = "red")
#    geom_hline(yintercept = 0, color = "black", lty = "dashed")
#    geom_hline(yintercept = -1.26, color = "black", lty = "dashed") 


filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        individual_difference
    ) %>% 
    write_clip()

top_bad <- filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        individual_difference
    ) %>% 
    head(10) %>% 
    pull(Municipalidad)

filtered_df %>% 
    filter(
        Municipalidad %in% top_bad
    ) %>% 
    select(Municipalidad, year, Avance) %>% 
    pivot_wider(names_from = year, values_from = Avance) %>% 
    clipr::write_clip()

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        desc(individual_difference)
    ) %>% head(10) %>% 
    write_clip()

top_good <- filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2015) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        desc(individual_difference)
    ) %>% 
    head(10) %>% 
    pull(Municipalidad)

filtered_df %>% 
    filter(
        Municipalidad %in% top_good
    ) %>% 
    select(Municipalidad, region, year, Avance) %>% 
    pivot_wider(names_from = year, values_from = Avance) %>% 
    clipr::write_clip()

#### 2018

###

filtered_df <- ejecucion_df %>% 
    filter(
        Municipalidad %in% (missings_df %>% 
                                filter(!filtered_from_sample) %>% 
                                pull(Municipalidad))
    )


filtered_df <- filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018),
           region = str_c(str_sub(UBIGEO, 1, 2), "0000"),
           provincia = str_c(str_sub(UBIGEO, 1, 4), "00")
    )

# 

filtered_df %>% 
    skimr::skim() %>% 
    write_clip()

filtered_df %>% 
    select(year, Avance) %>% 
    group_by(year) %>% 
    skim() %>% 
    write_clip()

filtered_df %>% 
    pivot_longer(-c(Municipalidad, year, EY, UBIGEO, region, provincia)) %>% 
    ggplot(
        aes(x = year, y = value, group = Municipalidad)
    ) +
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    facet_wrap(~name, ncol = 1, scales = "free_y") +
    theme_bw()

filtered_df %>% 
    group_by(Municipalidad, EY, UBIGEO) %>% 
    arrange(Municipalidad, UBIGEO, EY, year) %>% 
    mutate(
        perc_advance_yoy_avance = c(0, diff(Avance)),
        perc_advance_yoy_avance_canon = c(0, diff(Avance_CANON))
    ) %>% 
    select(-c(Avance, Avance_CANON)) %>% 
    pivot_longer(-c(Municipalidad, year, EY, UBIGEO, region, provincia)) %>% 
    ggplot(
        aes(x = year, y = value, group = Municipalidad)
    ) +
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    facet_wrap(~name + EY, ncol = 1) +
    theme_bw()

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018)) %>% 
    ggplot(
        aes(x = year, group = year, y = Avance)
    ) +
    geom_violin()

filtered_df %>% 
    rowwise() %>% 
    mutate(Avance = Avance/100) %>% 
    ggplot(
        aes(x = year, group = year, y = Avance)
    ) +
    geom_boxplot(notch = TRUE) +
    scale_x_continuous(breaks = 2006:2019) +
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Avance", x = "Año")

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment
    ) %>% 
    coeftest() %>% 
    tidy(vcov = vcovHAC) %>% 
    write_clip()

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment + EY
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018)) %>% 
    lm(data = .,
       formula = Avance_CANON ~ year + treatment + time_since_treatment + EY
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2018,
           time_since_treatment = max(0, year - 2018)) %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region
    ) %>%
    coeftest(vcov = sandwich::vcovHC) %>% 
    tidy() %>% 
    write_clip()

filtered_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region + provincia
    ) %>% 
    summary(vcov = vcovHAC)

filtered_df %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           region + provincia
    ) %>% 
    summary(vcov = vcovHAC)

ejecucion_panel = plm::pdata.frame(filtered_df, index = c("Municipalidad", "year"))
filtered_df %>% 
    plm(
        data = .,
        formula = Avance ~ treatment + time_since_treatment,
        model = "within", effect = "individual"
    ) %>% 
    coeftest(vcov = vcovHC) %>% 
    tidy() %>% 
    write_clip()

filtered_df %>% 
    group_by(year) %>% 
    summarise(
        mean_avance = mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = mean_avance)
    ) + 
    geom_point() +
    geom_line() +
    ylim(c(0, 100))

filtered_df %>% 
    group_by(year) %>% 
    summarise(
        mean_avance = mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = mean_avance)
    ) + 
    geom_point() +
    geom_line() +
    ylim(c(0, 100))

filtered_df %>% 
    group_by(Municipalidad) %>%
    mutate(
        individual_difference = Avance - mean(Avance, na.rm = TRUE)
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = Municipalidad)
    ) + 
    geom_point(alpha = 0.1) +
    geom_line(alpha = 0.1) +
    ylim(c(-100, 100))

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = Municipalidad)
    ) + 
    geom_point(alpha = 0.05) +
    geom_line(alpha = 0.05) +
    ylim(c(-100, 100)) +
    labs(x = "Año", y = "Diferencia",
         subtitle = "Diferencia entre la ejecución del año y la ejecución promedio pre-intervención")

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    group_by(year < 2018) %>% 
    summarise(
        mean_mean = mean(individual_difference, na.rm = TRUE)
    )

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>% 
    ggplot(
        aes(x = year, y = individual_difference, group = year)
    ) + 
    geom_jitter(alpha = 0.1, pch = ".") +
    geom_violin(alpha = 0.05) +
    ylim(c(-100, 100)) +
    labs(x = "Año", y = "Diferencia",
         subtitle = "Diferencia entre la ejecución del año y la ejecución promedio pre-intervención") +
    scale_x_continuous(breaks = 2007:2019) +
    geom_vline(xintercept = 2014.5, color = "red")
#    geom_hline(yintercept = 0, color = "black", lty = "dashed")
#    geom_hline(yintercept = -1.26, color = "black", lty = "dashed") 


filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        individual_difference
    ) %>% 
    write_clip()

top_bad <- filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        individual_difference
    ) %>% 
    head(10) %>% 
    pull(Municipalidad)

filtered_df %>% 
    filter(
        Municipalidad %in% top_bad
    ) %>% 
    select(Municipalidad, year, Avance) %>% 
    pivot_wider(names_from = year, values_from = Avance) %>% 
    clipr::write_clip()

filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        desc(individual_difference)
    ) %>% head(10) %>% 
    write_clip()

top_good <- filtered_df %>% 
    left_join(filtered_df %>% 
                  filter(year < 2018) %>% 
                  group_by(Municipalidad) %>% 
                  summarise(
                      pretreatment_mean = mean(Avance, na.rm = TRUE)
                  ),
              by = "Municipalidad") %>% 
    mutate(
        individual_difference = Avance - pretreatment_mean
    ) %>%
    filter(year > 2014) %>% 
    group_by(Municipalidad, region) %>% 
    summarise(
        individual_difference = median(individual_difference)
    ) %>% 
    arrange(
        desc(individual_difference)
    ) %>% 
    head(10) %>% 
    pull(Municipalidad)

filtered_df %>% 
    filter(
        Municipalidad %in% top_good
    ) %>% 
    select(Municipalidad, region, year, Avance) %>% 
    pivot_wider(names_from = year, values_from = Avance) %>% 
    clipr::write_clip()


## STARGAZER TABLES

library(stargazer)

geoperu_departamentos <- read_excel("data/01_raw/geodir-ubigeo-reniec.xlsx")

geoperu_departamentos <- geoperu_departamentos %>% 
    mutate(
        region = str_c(str_sub(Ubigeo, 1, 2), "0000"),
        provincia = str_c(str_sub(Ubigeo, 1, 4), "00")
    ) %>% 
    count(region, Departamento)

missings_df <- ejecucion_df %>% 
    mutate(total_years = n_distinct(year)) %>% 
    group_by(Municipalidad) %>% 
    summarise(
        available_years = n(),
        total_years = max(total_years),
        perc_na = 1 - n()/total_years,
        filtered_from_sample = perc_na > 0.5) %>% 
    arrange(desc(perc_na))

###

filtered_df <- ejecucion_df %>% 
    filter(
        Municipalidad %in% (missings_df %>% 
                                filter(!filtered_from_sample) %>% 
                                pull(Municipalidad))
    )


filtered_df <- filtered_df %>% 
    rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015),
           region = str_c(str_sub(UBIGEO, 1, 2), "0000"),
           provincia = str_c(str_sub(UBIGEO, 1, 4), "00")
    )

model_data <-  filtered_df %>% 
    left_join(
        geoperu_departamentos,
        by = "region"
    ) %>% rowwise() %>% 
    mutate(treatment = year >= 2015,
           time_since_treatment = max(0, year - 2015)) 

model_fe <- model_data %>% 
    lm(data = .,
       formula = Avance ~ year + treatment + time_since_treatment +
           Departamento
    )

cov1 <- sandwich::vcovHC(model_fe, order.by = filtered_df$year)
robust_se <- sqrt(diag(cov1))

stargazer(model_fe, type = "text",
          se = list(robust_se), out = "fixed_region_ITS_model.txt")

stargazer(model_fe, type = "latex",
          se = list(robust_se), out = "fixed_region_ITS_model.tex")

stargazer(model_fe, type = "html",
          se = list(robust_se), out = "fixed_region_ITS_model.doc")
