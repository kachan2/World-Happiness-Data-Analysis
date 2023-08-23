library(tidyverse)
library(dplyr)
library(reshape2)
library(corrplot)


# calculates the correlation matrix for a given country to be used
# for the correlation plot in the Other Graphs tab
calculateCorrelations = function(x, country) {
  x = x |> 
    filter(region == country) |>
    select(Score:Generosity)
  toReturn = round(cor(x), 2)
  return (toReturn)
}