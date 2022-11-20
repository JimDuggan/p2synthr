library(readr)
library(lubridate)
library(dplyr)
library(devtools)

fx <- read_csv("data-raw/p2_model_outputs.csv")

fx <- fx %>%
       rowwise() %>%
       mutate(Syn0024=rpois(n = 1,lambda = `Model 00-24`),
              Syn2549=rpois(n = 1,lambda = `Model 25-49`),
              Syn50P=rpois(n = 1,lambda = `Model 50P`)) %>%
       mutate(Scenario="Baseline") %>%
       select(Scenario,everything())


use_data(fx,overwrite = T)
