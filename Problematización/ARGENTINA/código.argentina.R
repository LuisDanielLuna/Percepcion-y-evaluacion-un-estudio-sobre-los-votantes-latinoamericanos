install.packages("tidyverse")
install.packages("haven")
install.packages("readxl")
library(tidyverse)
library(haven)
library(readxl)



#aprobación presidencial 

base_ap<- read_dta("ap.dta")
base_arg<- base_ap %>%
  filter(country == "Argentina") %>%
  select(country,year, month, net)
base_arg <- base_arg %>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))

promedios_trimestrales <- base_arg %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_arg <- base_arg %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))


write.csv("promedios_trimestrales.csv")
write.csv("promedios_trimestrales.csv")



