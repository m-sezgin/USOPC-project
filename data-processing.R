# PURPOSE: Process raw Olympic Triathlon data


# load packages -----------------------------------------------------------

library(tidyverse)
library(here)
library(rio)

# load data ---------------------------------------------------------------

# female triathlete olympic rankings
women <- import(here("raw-data/athlete_women.csv")) |>
  mutate(date = lubridate::ymd(date))

# male triathlete olympic rankings
men <- import(here("raw-data/athlete_men.csv")) |>
  mutate(date = lubridate::ymd(date))

# qualification event data
events <- import(here("raw-data/events.csv")) |>
  mutate(start_date = lubridate::ymd(start_date),
         end_date = lubridate::ymd(end_date))

# country mixed relay olympic rankings
country_ranking <- import(here("raw-data/country_ranking.csv")) |>
  mutate(date = lubridate::ymd(date))

# clean data --------------------------------------------------------------

# add variables to events dataset for data viz
event_clean<- events |>
  mutate(period = case_when(start_date %within% interval(lubridate::ymd("2022-05-27"), lubridate::ymd("2023-05-26")) ~ "May 27th 2022 - May 26th 2023",
                             TRUE ~ "May 27th 2023 - May 27th 2024"),
         automatic = case_when(event == "World Triathlon Mixed Relay Olympic Qualification Event" ~ "Yes",
                               event == "2022 World Triathlon Sprint & Relay Championships Montreal" ~ "Yes",
                               event == "2023 World Triathlon Sprint & Relay Championships Hamburg" ~ "Yes",
                               TRUE ~ "No"),
         )

# add variables to female triathlete dataset for data viz
women_clean <- women |>
  select(-current_sum) |>
  group_by(name) |>
  arrange(date) |>
  filter(currently_used == "Yes") |>
  mutate(cumulative = cumsum(points)) |>
  ungroup() |>
  mutate(american = ifelse(country_abb == "USA", "Yes", "No"),
         rank = as.character(paste0("# ", current_rank))
  )

# add variables to male triathlete dataset for data viz
men_clean <- men |>
  group_by(name) |>
  arrange(date) |>
  filter(currently_used == "Yes") |>
  mutate(cumulative = cumsum(points)) |>
  ungroup() |>
  mutate(american = ifelse(country_abbr == "USA", "Yes", "No"),
         rank = as.character(paste0("# ", current_rank)),
         qualification = ifelse(current_rank <= 26, "On Track to Qualify", "Not on Track to Qualify")
  ) |>
  filter(current_rank %in% c(1:26, 32, 42))

# add variables to country dataset for data viz
country_clean <- country_ranking |>
  mutate(qualification = case_when(country_abbr == "GBR" | country_abbr == "FRA" ~ "Qualified",
                                    country_abbr %in% c("GER", "NZL", "SUI", "AUS", "USA", "ITA", "POR") ~ "On Track to Qualify",
                                    TRUE ~ "Not on Track to Qualify")) |>
  group_by(country) |>
  arrange(date) |>
  filter(currently_used == "Yes") |>
  mutate(cumulative = cumsum(points)) 

# save datasets -----------------------------------------------------------

write_csv(women_clean, "processed-data/women_cumulative.csv")
write_csv(men_clean, "processed-data/men_cumulative.csv")
write_csv(event_clean, "processed-data/events_period.csv")
write_csv(country_clean, "processed-data/country_cumulative.csv")



