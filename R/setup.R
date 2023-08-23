library(tidyverse)
library(readr)
library(dplyr)

csv2015 = read_csv("./data/2015.csv") |> mutate ("Year" = 2015)
csv2016 = read_csv("./data/2016.csv") |> mutate ("Year" = 2016)
csv2017 = read_csv("./data/2017.csv") |> mutate ("Year" = 2017)
csv2018 = read_csv("./data/2018.csv") |> mutate ("Year" = 2018)
csv2019 = read_csv("./data/2019.csv") |> mutate ("Year" = 2019)
csv2020 = read_csv("./data/2020.csv") |> mutate ("Year" = 2020)
csv2021 = read_csv("./data/2021.csv") |> mutate ("Year" = 2021)
csv2022 = read_csv("./data/2022.csv") |> mutate("Year" = 2022)

iso = read_csv("./data/wikipedia-iso-country-codes.csv")
happiness = csv2015 |>
  bind_rows(csv2016) |>
  bind_rows(csv2017) |>
  bind_rows(csv2018) |>
  bind_rows(csv2019) |>
  bind_rows(csv2020) |>
  bind_rows(csv2021) |>
  bind_rows(csv2022) |>
  left_join(iso, by = "Country") |>
  rename(ISO3 = alpha3) |>
  select(Country:Generosity, Year, ISO3) |>
  arrange(Country)

write_csv(x = happiness, "data/happiness.csv")

