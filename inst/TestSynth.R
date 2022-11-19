library(dplyr)
library(ggplot2)
library(tidyr)

irl <- owid |>
        filter(location=="Ireland") |>
        pull(new_cases)
       
irl_syn <- synth1(irl,
                  group_names=c("00-19","20-39","40+"),
                  group_prob = c(0.20,0.40,0.40)) |>
           mutate(Day=1:length(irl)) |>
           select(Day,everything())

irl_tidy <- irl_syn |>
             pivot_longer(names_to="Age",
                          values_to="Incidence",
                          `00-19`:`40+`)

ggplot(irl_tidy,aes(x=Day,y=Incidence,fill=Age))+geom_area()
