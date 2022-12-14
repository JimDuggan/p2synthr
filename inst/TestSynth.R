library(dplyr)
library(ggplot2)
library(tidyr)
library(p2synthr)

# Some useful commands for package building
# devtools::document() for documentation
# devtools::check()
# devtools::build()
# devtools::install_github("JimDuggan/p2synthr")
# install.packages("p2synthr_0.0.1.tar.gz",repos = NULL,type="source")

irl <- owid |>
        filter(location=="Ireland") |>
        pull(new_cases)
       
irl_syn_age <- synth1(irl,
                  group_names=c("00-19","20-39","40+"),
                  group_prob = c(0.20,0.40,0.40))

irl_syn_gen <- synth1(irl,
                      group_names=c("Male","Female"),
                      group_prob = c(0.30,0.70))

irl_tidy <- irl_syn_age |>
               pivot_longer(names_to="Age",
                            values_to="Incidence",
                            `00-19`:`40+`)

p1 <- ggplot(irl_tidy,aes(x=Index,y=Incidence,fill=Age))+geom_area()+ggtitle("Synthetic Data")
