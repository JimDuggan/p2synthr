library(p2synthr)
library(dplyr)
library(ggplot2)
library(tidyr)

irl <- owid |>
  filter(location=="United Kingdom") |>
  select(date,new_cases)

synth_var <- synth_variants(pull(irl,new_cases),
                            pull(irl,date),
                            percent_genomic_surveillance_min = 0.7,
                            percent_genomic_surveillance_max = 0.9)

t <- synth_var %>%
       select(Date,TotalCases,starts_with("SynCases"))

t_p <- t %>%
        pivot_longer(names_to="Variant",
                     values_to="Cases",
                     -c(Date,TotalCases))

p1 <- ggplot(t_p,aes(x=Date,y=Cases,fill=Variant))+geom_area()+
      theme(legend.position="top")


v <- synth_var %>%
  select(Date,ends_with("Fr"))

v_p <- v %>%
  pivot_longer(names_to="Variant",
               values_to="Fraction",
               -c(Date))

p2 <- ggplot(v_p,aes(x=Date,y=Fraction,fill=Variant))+geom_area()+
      theme(legend.position="top")
