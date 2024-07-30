## No identificados
## Caso 1: Sin liderazgo, baja instituciinalización
resultados1 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="0", nuli=0, ins= 39.6), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso1 <- subset(resultados1, results$factor == "juicio_economico3")

## Caso 2: Sin liderazgo, alta instituciinalización
resultados2 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="0", nuli=0, ins= 14.2), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso2 <- subset(resultados2, results$factor == "juicio_economico3")

casosnoident<-rbind(caso1,caso2)

casosnoident$institucionalizacion <- ifelse(casosnoident$ins == 39.6, "baja", "alta")

p1 <- ggplot(casosnoident, aes(x= institucionalizacion, y = AME, color = institucionalizacion)) +
  geom_point(size = 3) +  # Punto para el efecto puntual
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Barras de error para el intervalo de confianza
  labs(
    title = "Escenario de no identifcados y sin liderazgo ",
    x = "Institucionalización",
    y = "voto económico",
    color = "Institucionalización") +
  ylim(-0.4, 0) +
  theme_minimal()
p1

## No identificados
## Caso 3: Con liderazgo, baja instituciinalización
resultados3 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="0", nuli=1, ins= 39.6), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso3 <- subset(resultados3, results$factor == "juicio_economico3")

## Caso 4: Con liderazgo, alta instituciinalización
resultados4 <- modelo_lmer %>%
  margins(
    variables= "juicio_economico",
    at= list(id="0", nuli=1, ins= 14.2), 
    vcov_fun= "vcovCR", vcov_type= "CR2", 
    vcov_args= list(cluster= base_modelo$pais_Año)
  ) %>%
  summary()

caso4 <- subset(resultados4, results$factor == "juicio_economico3")

casosnoident.2<-rbind(caso3,caso4)

casosnoident.2$institucionalizacion <- ifelse(casosnoident.2$ins == 39.6, "baja", "alta")

p2 <- ggplot(casosnoident.2, aes(x= institucionalizacion, y = AME, color = institucionalizacion)) +
  geom_point(size = 3) +  # Punto para el efecto puntual
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Barras de error para el intervalo de confianza
  labs(
    title = "Escenario de no identifcados y con liderazgo ",
    x = "Institucionalización",
    y = "voto económico",
    color = "Institucionalización") +
  ylim(-0.4, 0) + 
  theme_minimal()

library(ggpubr)
ggarrange(p1, p2)
p <- ggarrange(p1 + theme(legend.position = "none"),  # Elimina la leyenda del primer plot
               p2 + theme(legend.position = "right"), # Muestra la leyenda al lado derecho del segundo plot
               ncol = 2, nrow = 1)
p





