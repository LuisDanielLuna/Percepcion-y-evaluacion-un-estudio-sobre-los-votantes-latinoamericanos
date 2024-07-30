## Identificados
## Caso 5: Sin liderazgo, baja instituciinalización
resultados5 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="1", nuli=0, ins= 39.6), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso5<- subset(resultados5, results$factor == "juicio_economico3")

## Caso 6: Sin liderazgo, alta instituciinalización
resultados6 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="1", nuli=0, ins= 14.2), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso6 <- subset(resultados6, results$factor == "juicio_economico3")

casosident<-rbind(caso5,caso6)

casosident$institucionalizacion <- ifelse(casosnoident$ins == 39.6, "baja", "alta")

p3 <- ggplot(casosident, aes(x= institucionalizacion, y = AME, color = institucionalizacion)) +
  geom_point(size = 3) +  # Punto para el efecto puntual
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Barras de error para el intervalo de confianza
  labs(
    title = "Escenario de identifcados y sin liderazgo ",
    x = "Institucionalización",
    y = "voto económico",
    color = "Institucionalización") +
  ylim(-0.4, 0) +
  theme_minimal()
p3

## identificados
## Caso 7: Con liderazgo, baja instituciinalización
resultados7 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="1", nuli=1, ins= 39.6), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso7<- subset(resultados7, results$factor == "juicio_economico3")

## Caso 8: Con liderazgo, alta instituciinalización
resultados8 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="1", nuli=1, ins= 14.2), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso8 <- subset(resultados8, results$factor == "juicio_economico3")

casosident.2<-rbind(caso7,caso8)

casosident.2$institucionalizacion <- ifelse(casosnoident.2$ins == 39.6, "baja", "alta")

p4 <- ggplot(casosident.2, aes(x= institucionalizacion, y = AME, color = institucionalizacion)) +
  geom_point(size = 3) +  # Punto para el efecto puntual
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Barras de error para el intervalo de confianza
  labs(
    title = "Escenario  identifcados y con liderazgo ",
    x = "Institucionalización",
    y = "voto económico",
    color = "Institucionalización") +
  ylim(-0.4, 0) +
  theme_minimal()

library(ggpubr)
ggarrange(p3, p4)

p_b <- ggarrange(p3 + theme(legend.position = "none"),  # Elimina la leyenda del primer plot
               p4+ theme(legend.position = "right"), # Muestra la leyenda al lado derecho del segundo plot
               ncol = 2, nrow = 1)
p_b
