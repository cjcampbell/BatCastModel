# Setup -------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggpubr)

# Load weather data -------------------------------------------------------
df <- read.csv("data/2020weatherData.csv") %>%
  distinct() %>%
  dplyr::mutate(
    day = lubridate::mdy(Date.time),
    yday = lubridate::yday(day)) %>%
  arrange(yday)

# Simulate bat emergences -------------------------------------------------
# Peak population is 500k
# 100k males, 250k females, 150k juveniles
set.seed(69); df_sim <- df %>%
  dplyr::mutate(
    # Incorporate birth and migration:
    pop_males = 100e3, # No males migrate
    pop_females = case_when(
      yday >= 90 & yday < 300 ~ 250e3, # All females migrate
      yday >= 300 & yday < 330 ~ 50e3,
      TRUE ~ 0
      ) ,
    pop_juvenilies = case_when(
      yday < 153                ~ 0     , # Babies are born on June 1!
      yday >= 153 & yday <= 300 ~ 150e3 ,
      yday > 300                ~ 75e3 # Half of juveniles migrate
      ),
    # Introduce multipliers:
    emergence_multiplier = 1, # Baseline is all individuals come out.
    # Temperature constraints:
    emergence_multiplier = case_when(
      Minimum.Temperature < 40 ~ emergence_multiplier * 0,
      Minimum.Temperature >= 40 & Minimum.Temperature < 50 ~ emergence_multiplier * 0.1,
      Minimum.Temperature >= 50 & Minimum.Temperature < 60 ~ emergence_multiplier * 0.33,
      Minimum.Temperature >= 60 & Minimum.Temperature < 70 ~ emergence_multiplier * 0.8,
      TRUE ~ emergence_multiplier
    ),
    emergence_multiplier = case_when(
      Maximum.Temperature < 69  ~ emergence_multiplier*0.01,
      Maximum.Temperature > 100 ~ emergence_multiplier*0.01,
      TRUE ~ emergence_multiplier
    ),
    emergence_multiplier = case_when(
      Heat.Index > 100 & Heat.Index <= 105 ~ emergence_multiplier*0.80,
      Heat.Index > 105 ~ emergence_multiplier*0.01,
      TRUE ~ emergence_multiplier
    ),
    # Wind constraints:
    emergence_multiplier = case_when(
      Wind.Speed >= 10 & Wind.Speed < 15 ~ emergence_multiplier * 0.95,
      Wind.Speed >= 15 & Wind.Speed < 17 ~ emergence_multiplier * 0.5,
      Wind.Speed >= 17 & Wind.Speed < 20 ~ emergence_multiplier * 0.1,
      Wind.Speed >=  20 ~ emergence_multiplier * 0.01,
      TRUE ~ emergence_multiplier
    ),
    # Precip constraints:
    emergence_multiplier = case_when(
      Precipitation >= .01 & Precipitation < .02 ~ emergence_multiplier * 0.95,
      Precipitation >= .02 & Precipitation < .05 ~ emergence_multiplier * 0.8,
      Precipitation >= .05 & Precipitation < 1 ~ emergence_multiplier * 0.45,
      Precipitation >=  1 ~ emergence_multiplier * 0.01,
      TRUE ~ emergence_multiplier
    ),
    # Juvenile development multipliers:
    juvenile_multiplier = case_when(
      yday < 153 + 28 ~ emergence_multiplier*0,
      yday >= 153 + 28 & yday <= 153 + 28*2 ~ emergence_multiplier*0.5 , # Half are foraging at 2 months
      yday >= 153 + 28*2 & yday <= 153 + 28*4 ~ emergence_multiplier*0.8 , # Most are foraging 3-4 months
      yday > 153 + 28*4 ~ emergence_multiplier # all are foraging at 4 months
      ),
    # Incorporate multipliers and simulate emergence
    cominOut_males = pop_males * (emergence_multiplier * rnorm(n = nrow(df), mean = 1, sd = 0.1)),
    cominOut_females = pop_females * (emergence_multiplier * rnorm(n = nrow(df), mean = 1, sd = 0.1)),
    cominOut_juvies =  pop_juvenilies* (juvenile_multiplier * rnorm(n = nrow(df), mean = 1, sd = 0.1)),
    emergence = round( cominOut_males + cominOut_females + cominOut_juvies )
  )

# Make a plot -------------------------------------------------------------

ggarrange(ncol = 1,
  plotlist = list(
    df_sim %>% ggplot() + aes(x=yday) +
      ylab("Precipitation (in)") + xlab(NULL) +
      geom_path(aes(y = Precipitation), color = "grey50") +
      ggpubr::theme_pubclean(),
    df_sim %>% ggplot() + aes(x=yday) +
      ylab("Wind Speed (mph)") + xlab(NULL) +
      geom_path(aes(y = Wind.Speed), color = "grey30") +
      geom_path(aes(y = Wind.Gust), color = "grey30", linetype = 1, size = 0.2) +
      ggpubr::theme_pubclean(),
    df_sim %>% ggplot() + aes(x=yday) +
      ylab("Temperature (F)") + xlab(NULL) +
      geom_path(aes(y = Maximum.Temperature), color = "red") +
      geom_path(aes(y = Heat.Index), color = "red", linetype = 1, size = 0.2) +
      geom_path(aes(y = Minimum.Temperature), color = "blue") +
      ggpubr::theme_pubclean(),
    df_sim %>% ggplot() + aes(x=yday, y = emergence) +
      geom_path() + ggpubr::theme_pubclean() +
      ylab("Emergence (# bats)") + xlab("Day of Year")
))


# Export -----------------------------------------------------------------

df_counts <- df_sim %>% dplyr::select_at(vars(-contains("multiplier"), -contains("pop"), -contains("cominOut")))

write.csv(df_counts, file = "data/simulatedEmergenceCounts.csv")
