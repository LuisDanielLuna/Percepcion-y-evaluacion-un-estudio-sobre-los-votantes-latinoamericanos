
############################################################################################################################
### Script para trabajar la relación entre estado de la situación económica y la aprobación presidencial (Teoría del voto económico)
###   en América Latina. Se analiza la hipótesis recompensa-castigo.
### Created: Luis Daniel Luna
### Modificado:02-07-2024
### Es necesario retomar las bases de datos del dataset "Problematización" en la 
###  cual se encuentran los datos  la aprobación presidencial ("aprobación.dta"), como las tasas trimestrales del PIB por paises (promedios_trimestrales_pais)        
###
############################################################################################################################
 
library(tidyverse)
library(haven)
library(readxl)
library(ggrepel)
library(gridExtra)
library(cowplot)
library(ggplot2)
library(patchwork)

#Análisis por países.

#1.ARGENTINA

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

write.csv(promedios_trimestrales, "promedios_trimestralesarg.csv")


#cruce entre la aprobación y crecimiento 
argentina<-read_xlsx("argentina1.xlsx")
modelo_arg<-lm(aprobacion~crecimiento, data=argentina)
summary(modelo_arg)

graficaarg<-ggplot(data=modelo_arg, aes(y = aprobacion,x = crecimiento))
graficaarg+geom_point()
graficaarg+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Argentina")

library(ggplot2)
graficaarg <- ggplot(data=modelo_arg, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Argentina")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficaarg)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_argentina.png", plot = graficaarg, width = 10, height = 8, units = "cm")




  
#2. BOLIVIA 

#aprobación presidencial 

base_ap<- read_dta("ap.dta")
base_bol<- base_ap %>%
  filter(country == "Bolivia") %>%
  select(country,year, month, net)
base_bol <- base_bol %>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))

promedios_trimestralesbol <- base_bol %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_bol <- base_bol %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))

write.csv(promedios_trimestrales, "promedios_trimestralesbol.csv")



#cruce entre la aprobación y crecimiento 
bolivia<-read_xlsx("bolivia1.xlsx")
modelo_bol<-lm(aprobacion~crecimiento, data=bolivia)
summary(modelo_bol)

graficabol<-ggplot(data=modelo_bol, aes(y = aprobacion,x = crecimiento))
graficabol+geom_point()
graficabol+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Bolivia")

library(ggplot2)
# Asegúrate de que el data frame modelo_bol tiene las columnas aprobacion y crecimiento.

graficabol <- ggplot(data=modelo_bol, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Bolivia")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficabol)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_bolivia.png", plot = graficabol, width = 10, height = 8, units = "cm")




#3 Brasil 
#aprobación presidencial 

base_ap<- read_dta("ap.dta")
base_bra<- base_ap %>%
  filter(country == "Brazil") %>%
  select(country,year, month, net)
base_bra <- base_bra%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesbra <- base_bra %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_bra <- base_bra %>%
  left_join(promedios_trimestralesbra, by = c("year", "trimestre"))

write.csv(promedios_trimestrales, "promedios_trimestralesbra.csv")

#cruce entre la aprobación y crecimiento 
brasil<-read_xlsx("brasil1.xlsx")
modelo_bra<-lm(aprobacion~crecimiento, data=brasil)
summary(modelo_bra)

graficabra<-ggplot(data=modelo_bra, aes(y = aprobacion,x = crecimiento))
graficabra+geom_point()
graficabra+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Brasil")


library(ggplot2)

graficabra <- ggplot(data=modelo_bra, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Brasil")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficabra)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_brasil.png", plot = graficabra, width = 10, height = 8, units = "cm")



# 4. CHILE
#aprobación presidencial 

base_ap<- read_dta("ap.dta")
base_chi<- base_ap %>%
  filter(country == "Chile") %>%
  select(country,year, month, net)
base_chi <- base_chi%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestraleschi <- base_chi %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_chi <- base_chi %>%
  left_join(promedios_trimestraleschi, by = c("year", "trimestre"))

write.csv(promedios_trimestraleschi, "promedios_trimestraleschi.csv")

#cruce entre la aprobación y crecimiento 
chile<-read_xlsx("chile1.xlsx")
modelo_chi<-lm(aprobacion~crecimiento, data=chile)
summary(modelo_chi)

graficachi<-ggplot(data=modelo_chi, aes(y = aprobacion,x = crecimiento))
graficachi+geom_point()
graficachi+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Chile")

library(ggplot2)

graficachi <- ggplot(data=modelo_chi, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Chile")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficachi)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_chile.png", plot = graficachi, width = 10, height = 8, units = "cm")



#5.COLOMBIA


#aprobación presidencial 

base_ap<- read_dta("ap.dta")
base_col<- base_ap %>%
  filter(country == "Colombia") %>%
  select(country,year, month, net)
base_col <- base_col%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralescol <- base_col %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_col <- base_col %>%
  left_join(promedios_trimestralescol, by = c("year", "trimestre"))

write.csv(promedios_trimestralescol, "promedios_trimestralesco1l.csv")

#cruce entre la aprobación y crecimiento 
colombia<-read_xlsx("colombia1.xlsx")
modelo_col<-lm(aprobacion~crecimiento, data=colombia)
summary(modelo_col)

graficacol<-ggplot(data=modelo_col, aes(y = aprobacion,x = crecimiento))
graficacol+geom_point()
graficacol+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Colombia")

library(ggplot2)

graficacol <- ggplot(data=modelo_col, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Colombia")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficacol)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_colombia.png", plot = graficacol, width = 10, height = 8, units = "cm")

  
#7. COSTA RICA

base_ap<- read_dta("ap.dta")
base_cos<- base_ap %>%
  filter(country == "Costa Rica") %>%
  select(country,year, month, net)
base_cos <- base_cos%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralescos <- base_cos %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_cos <- base_cos %>%
  left_join(promedios_trimestralescos, by = c("year", "trimestre"))

write.csv(promedios_trimestralescos, "promedios_trimestralescos.csv")

#cruce entre la aprobación y crecimiento 
costa<-read_xlsx("costarica1.xlsx")
modelo_cos<-lm(aprobacion~crecimiento, data=costa)
summary(modelo_cos)

graficacol<-ggplot(data=modelo_cos, aes(y = aprobacion,x = crecimiento))
graficacol+geom_point()
graficacol+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Costa Rica")

library(ggplot2)


graficacos <- ggplot(data=modelo_cos, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Costa Rica")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficacos)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_costa_rica.png", plot = graficacos, width = 10, height = 8, units = "cm")


#8. Ecuador

base_ap<- read_dta("ap.dta")
base_ecu<- base_ap %>%
  filter(country == "Ecuador") %>%
  select(country,year, month, net)
base_ecu <- base_ecu%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesecu <- base_ecu %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_ecu <- base_ecu %>%
  left_join(promedios_trimestralesecu, by = c("year", "trimestre"))

write.csv(promedios_trimestralesecu, "promedios_trimestralesecu.csv")

#cruce entre la aprobación y crecimiento 
ecu<-read_xlsx("ecu1.xlsx")
modelo_ecu<-lm(aprobacion~crecimiento, data=ecu)
summary(modelo_ecu)

graficaecu<-ggplot(data=modelo_ecu, aes(y = aprobacion,x = crecimiento))
graficaecu+geom_point()
graficaecu+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Ecuador")

library(ggplot2)

graficaecu <- ggplot(data=modelo_ecu, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Ecuador")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficaecu)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_ecuador.png", plot = graficaecu, width = 10, height = 8, units = "cm")



#9.El Salvador

base_ap<- read_dta("ap.dta")
base_es<- base_ap %>%
  filter(country == "El Salvador") %>%
  select(country,year, month, net)
base_es <- base_es%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestraleses <- base_es %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_es <- base_es %>%
  left_join(promedios_trimestraleses, by = c("year", "trimestre"))
write.csv(promedios_trimestraleses, "promedios_trimestraleses.csv")

#cruce entre la aprobación y crecimiento 
es<-read_xlsx("es1.xlsx")
modelo_es<-lm(aprobacion~crecimiento, data=es)
summary(modelo_es)

graficaes<-ggplot(data=modelo_es, aes(y = aprobacion,x = crecimiento))
graficaes+geom_point()
graficaes+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("El Salvador")

library(ggplot2)

graficaes <- ggplot(data=modelo_es, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("El Salvador")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficaes)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_el_salvador.png", plot = graficaes, width = 10, height = 8, units = "cm")



#10. GUATEMALA
base_ap<- read_dta("ap.dta")
base_gu<- base_ap %>%
  filter(country == "Guatemala") %>%
  select(country,year, month, net)
base_gu <- base_gu%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesgu <- base_gu %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_gu <- base_gu %>%
  left_join(promedios_trimestralesgu, by = c("year", "trimestre"))
write.csv(promedios_trimestralesgu, "promedios_trimestralesgu.csv")

#cruce entre la aprobación y crecimiento 
gu<-read_xlsx("gu.xlsx")
modelo_gu<-lm(aprobacion~crecimiento, data=gu)
summary(modelo_gu)

graficagu<-ggplot(data=modelo_gu, aes(y = aprobacion,x = crecimiento))
graficagu+geom_point()
graficagu+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Guatemala")

library(ggplot2)
graficagu <- ggplot(data=modelo_gu, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Guatemala")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficagu)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_guatemala.png", plot = graficagu, width = 10, height = 8, units = "cm")


#11. HONDURAS

base_ap<- read_dta("ap.dta")
base_h<- base_ap %>%
  filter(country == "Honduras") %>%
  select(country,year, month, net)
base_h <- base_h%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesh<- base_h %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_h <- base_h %>%
  left_join(promedios_trimestralesh, by = c("year", "trimestre"))
write.csv(promedios_trimestralesh, "promedios_trimestralesh.csv")

h<-read_xlsx("h1.xlsx")
modelo_h<-lm(aprobacion~crecimiento, data=h)
summary(modelo_h)

graficahon<-ggplot(data=modelo_h, aes(y = aprobacion,x = crecimiento))
graficahon+geom_point()
graficahon+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Honduras")

library(ggplot2)

graficahon <- ggplot(data=modelo_h, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Honduras")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficahon)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_honduras.png", plot = graficahon, width = 10, height = 8, units = "cm")


view

imagen_combinada <- grid.arrange(c,a,ncol=2)


#MÉXICO

base_ap<- read_dta("ap.dta")
base_mx<- base_ap %>%
  filter(country == "Mexico") %>%
  select(country,year, month, net)
base_mx <- base_mx%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesm<- base_mx %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_mx <- base_mx %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralesh, "promedios_trimestralesm.csv")

mexico<-read_xlsx("mx1.xlsx")
modelo_mx<-lm(aprobacion~crecimiento, data=mexico)
summary(modelo_mx)

graficamx<-ggplot(data=modelo_mx, aes(y = aprobacion,x = crecimiento))
graficamx+geom_point()
graficamx+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("México")

library(ggplot2)

graficamx <- ggplot(data=modelo_mx, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("México")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficamx)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_mexico.png", plot = graficamx, width = 10, height = 8, units = "cm")




#NICARAGUA

base_ap<- read_dta("ap.dta")
base_ni<- base_ap %>%
  filter(country == "Nicaragua") %>%
  select(country,year, month, net)
base_ni <- base_ni%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesni<- base_ni %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_ni <- base_ni %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralesni, "promedios_trimestralesni.csv")
write.csv(promedios_trimestralesni, "promedios_trimestralesni.csv")

nicaragua<-read_xlsx("ni1.xlsx")
modelo_ni<-lm(aprobacion~crecimiento, data=nicaragua)
summary(modelo_h)

graficani<-ggplot(data=modelo_ni, aes(y = aprobacion,x = crecimiento))
graficani+geom_point()
graficani+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Nicaragua")

library(ggplot2)

graficani <- ggplot(data=modelo_ni, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añade puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añade línea de ajuste lineal
  ggtitle("Nicaragua")  # Añade título

# Imprime la gráfica en la consola (necesario para scripts o funciones)
print(graficani)

# Guarda la gráfica en un archivo PNG
ggsave("grafica_nicaragua.png", plot = graficani, width = 10, height = 8, units = "cm")



#PANAMÁ

base_ap<- read_dta("ap.dta")
base_pa<- base_ap %>%
  filter(country == "Panama") %>%
  select(country,year, month, net)
base_pa <- base_pa%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralespa<- base_pa %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_pa <- base_pa %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralespa, "promedios_trimestralespa.csv")


panama<-read_xlsx("pan1.xlsx")
modelo_pa<-lm(aprobacion~crecimiento, data=panama)
summary(modelo_pa)

graficapa<-ggplot(data=modelo_pa, aes(y = aprobacion,x = crecimiento))
graficapa+geom_point()
graficapa+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Panamá")

library(ggplot2)

graficapa <- ggplot(data=modelo_pa, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añadir puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añadir línea de ajuste lineal
  ggtitle("Panamá")  # Añadir título

# Imprimir la gráfica en la consola (necesario para scripts o funciones)
print(graficapa)

# Guardar la gráfica en un archivo PNG
ggsave("grafica_panama.png", plot = graficapa, width = 10, height = 8, units = "cm")


#PERÚ

base_ap<- read_dta("ap.dta")
base_pe<- base_ap %>%
  filter(country == "Peru") %>%
  select(country,year, month, net)
base_pe <- base_pa%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralespe<- base_pe %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_pe <- base_pe %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralespe, "promedios_trimestralespe.csv")


peru<-read_xlsx("pe1.xlsx")
modelo_pe<-lm(aprobacion~crecimiento, data=peru)
summary(modelo_pa)

graficape<-ggplot(data=modelo_pe, aes(y = aprobacion,x = crecimiento))
graficape+geom_point()
graficape+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Perú")



library(ggplot2)

graficape <- ggplot(data=modelo_pe, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añadir puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añadir línea de ajuste lineal
  ggtitle("Perú")  # Añadir título

# Imprimir la gráfica en la consola (necesario para scripts o funciones)
print(graficape)

# Guardar la gráfica en un archivo PNG
ggsave("grafica_peru.png", plot = graficape, width = 10, height = 8, units = "cm")


#REPÚBLICA DOMINICANA

base_ap<- read_dta("ap.dta")
base_rd<- base_ap %>%
  filter(country == "Dominican Republic") %>%
  select(country,year, month, net)
base_rd <- base_rd%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesrd<- base_rd %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_rd <- base_rd %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralesrd, "promedios_trimestralesrd.csv")


rd<-read_xlsx("rd1.xlsx")
modelo_rd<-lm(aprobacion~crecimiento, data=rd)
summary(modelo_pa)

graficard<-ggplot(data=modelo_rd, aes(y = aprobacion,x = crecimiento))
graficard+geom_point()
graficard+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("República Dominicana")

library(ggplot2)

graficard <- ggplot(data=modelo_rd, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +  # Añadir puntos de dispersión
  geom_smooth(method= "lm", colour="red") +  # Añadir línea de ajuste lineal
  ggtitle("República Dominicana")  # Añadir título

# Imprimir la gráfica en la consola (necesario para scripts o funciones)
print(graficard)

# Guardar la gráfica en un archivo PNG
ggsave("grafica_republica_dominicana.png", plot = graficard, width = 10, height = 8, units = "cm")



#PARAGUAY

base_ap<- read_dta("ap.dta")
base_par<- base_ap %>%
  filter(country == "Paraguay") %>%
  select(country,year, month, net)
base_par <- base_par%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralespar<- base_par %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_par <- base_par %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralesrd, "promedios_trimestralespar.csv")


paraguay<-read_xlsx("par1.xlsx")
modelo_par<-lm(aprobacion~crecimiento, data=paraguay)
summary(modelo_par)

graficapar<-ggplot(data=modelo_par, aes(y = aprobacion,x = crecimiento))
graficapar+geom_point()
graficapar+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Paraguay")

library(ggplot2)

graficapar <- ggplot(data=modelo_par, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +
  geom_smooth(method= "lm", colour="red") +
  ggtitle("Paraguay")

# Ahora imprime y guarda la gráfica como una imagen
print(graficapar)
ggsave("grafica_paraguay.png", plot = graficapar, width = 10, height = 8, units = "cm")



#URUGUAY

base_ap<- read_dta("ap.dta")
base_ur<- base_ap %>%
  filter(country == "Uruguay") %>%
  select(country,year, month, net)
base_ur <- base_ur%>%
  mutate(trimestre = case_when(
    month %in% c(1, 2, 3) ~ "Q1",
    month %in% c(4, 5, 6) ~ "Q2",
    month %in% c(7, 8, 9) ~ "Q3",
    month %in% c(10, 11, 12) ~ "Q4"
  ))
promedios_trimestralesur<- base_ur %>%
  group_by(year, trimestre) %>%
  summarise(promedio_net = mean(net, na.rm = TRUE)) %>%
  ungroup() 

base_ur <- base_ur %>%
  left_join(promedios_trimestrales, by = c("year", "trimestre"))
write.csv(promedios_trimestralesur, "promedios_trimestralesur.csv")


uruguay<-read_xlsx("uru1.xlsx")
modelo_ur<-lm(aprobacion~crecimiento, data=uruguay)
summary(modelo_par)

graficaur<-ggplot(data=modelo_ur, aes(y = aprobacion,x = crecimiento))+
graficaur+geom_point()+
graficaur+geom_point()+geom_smooth(method= "lm", colour="red")+
  ggtitle("Uruguay")
print(graficaur)


library(ggplot2)

graficaur <- ggplot(data=modelo_ur, aes(y = aprobacion, x = crecimiento)) +
  geom_point() +
  geom_smooth(method= "lm", colour="red") +
  ggtitle("Uruguay")

# Ahora imprime y guarda la gráfica como una imagen
print(graficaur)
ggsave("grafica_uruguay.png", plot = graficaur, width = 10, height = 8, units = "cm")



#UNIÓN DE LOS GRÁFICOS DE DISPERSIÓN


layout_graficas <- (graficaarg| graficabol| graficabra| graficachi) /
  (graficacol| graficacos| graficaecu|graficaes)/
  ( graficagu|graficahon|graficamx |graficani)/
  ( graficapa|graficapar|graficape|graficard)/
  (graficaur|grafica_vacia | grafica_vacia | grafica_vacia)
print(layout_graficas)

grafica_vacia <- ggplot() + theme_void()

# Ajusta tu layout para un diseño 4x4
layout_graficas <- (graficaarg | graficabol | graficabra | graficachi) /
  (graficacol | graficacos | graficaecu | graficaes) /
  (graficagu | graficahon | graficamx | graficani) /
  (graficapa | graficapar | graficape | graficaur) / 
  (grafica_vacia | grafica_vacia | grafica_vacia | grafica_vacia)
print(layout_graficas)




summary(modelo_arg)
summary(modelo_bol)
summary(modelo_bra)
summary(modelo_chi)
summary(modelo_col)
summary(modelo_cos)
summary(modelo_ecu)
summary(modelo_es)
summary(modelo_gu)
summary(modelo_h)
summary(modelo_mx)
summary(modelo_ni)
summary(modelo_pa)
summary(modelo_par)
summary(modelo_pe)
summary(modelo_rd)
summary(modelo_ur)






