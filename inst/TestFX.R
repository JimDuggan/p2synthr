library(dplyr)
library(ggplot2)
library(tidyr)
library(p2synthr)


epi <- fx |>
        select(Time,Synthetic_incidence_A:Synthetic_incidence_C) |>
               pivot_longer(names_to="TSVariable",
                            values_to="Incidence",
                            -Time)

p1 <- ggplot(epi,
             aes(x=Time,y=Incidence,colour=TSVariable))+
             geom_line()+ggtitle("Synthetic EPI Data")

hos <- fx |>
  select(Time,Synthetic_hospitalisation_A:Synthetic_hospitalisation_C) |>
  pivot_longer(names_to="TSVariable",
               values_to="Incidence",
               -Time)

p2 <- ggplot(hos,
             aes(x=Time,y=Incidence,colour=TSVariable))+
  geom_line()+ggtitle("Synthetic Hospitalisation Data")