library(tidyverse)
#Cargar la base de datos 
base_modelo<- read_csv("base_final.csv")


# Convertir variables a los tipos de datos correctos

base_modelo <- base_modelo %>%
  rename(ideologia = posicion_partido)
base_modelo$intencion_voto <- as.numeric(base_modelo$intencion_voto)
base_modelo$juicio_economico <- factor(base_modelo$juicio_economico)
base_modelo$id <- factor(base_modelo$id)
base_modelo$ins <- as.numeric(base_modelo$ins)
base_modelo$liderazgo_caris <- factor(base_modelo$liderazgo_caris)
base_modelo$edad <- as.numeric(base_modelo$edad)
base_modelo$crecimiento <- as.numeric(base_modelo$crecimiento)
base_modelo$reelección <- factor(base_modelo$reelección)
base_modelo$ideología <- factor(base_modelo$ideologia)
base_modelo$voto_pasado <- factor(base_modelo$voto_pasado)
base_modelo$genero <- factor(base_modelo$genero)
base_modelo$Año <- factor(base_modelo$Año)

# Crear la variable combinada pais_Año como factor
base_modelo$pais_Año <- factor(paste(base_modelo$pais, base_modelo$Año, sep = "_"))

# variabel nuli para los líderes carismáticos 
base_modelo$nuli <- 0

# Asignar 1 a las observaciones que cumplen las condiciones
base_modelo$nuli[base_modelo$pais == "Argentina" & base_modelo$Año %in% c(2008, 2010)] <- 1
base_modelo$nuli[base_modelo$pais == "Bolivia"] <- 1
base_modelo$nuli[base_modelo$pais == "México" & base_modelo$Año == 2018] <- 1
base_modelo$nuli[base_modelo$pais == "Venezuela"] <- 1


# Guardar el archivo actualizado
write_csv(base_modelo, "base_completa_actualizada.csv")
# AJUSTE MODELO LMER
library(lme4)
modelo_lmer <- lmer(intencion_voto ~ juicio_economico * id * ins *nuli +
                      percepcion_inseguridad + percepcion_corrupcion + 
                      reelección + ideologia + 
                      edad + Año + crecimiento + voto_pasado+
                      (1 | pais_Año), 
                    data = base_modelo)
options(scipen = 999)

summary(modelo_lmer)

#ERRORES ESTÁNDAR ROBUSTOS
library(clubSandwich)
coef_robust <- coef_test(modelo_lmer, vcov = "CR2")

library(stargazer)

capture_output <- as.data.frame(print(coef_robust))


stargazer(capture_output, type = "text", summary = FALSE, 
          star.cutoffs = c(0.05, 0.01, 0.001), 
          notes = c("Significance levels: *** p<0.001, ** p<0.01, * p<0.05"), 
          out.header = FALSE)
html_output <- stargazer(capture_output, type = "html", summary = FALSE, 
                         star.cutoffs = c(0.05, 0.01, 0.001), 
                         notes = c("Significance levels: *** p<0.001, ** p<0.01, * p<0.05"), 
                         out.header = FALSE)

# Guardar la salida HTML en un archivo
cat(html_output, file = "tabla20241.html")



# Filtrar los coeficientes de interés del objeto coef_robust para crear gráfico de coefcientes
coef_interes <- capture_output %>%
  filter(Coef.%in% c("juicio_economico3", "id1", "ins", "nuli", 
                     "juicio_economico:id1", "juicio_economico3:ins",
                     "juicio_economico3:nuli", "id1:ins", 
                     "id1:nuli", "ins:nuli", "juicio_economico3:id1:ins", 
                     "juicio_economico3:id1:nuli", "juicio_economico3:ins:nuli",
                     "id1:ins:nuli", "juicio_economico3:id1:ins:nuli"))

coef_interes <- capture_output %>%
  filter(Coef. %in% c("juicio_economico3", "id1", "ins", "nuli", 
                      "juicio_economico3:id1", "juicio_economico3:ins",
                      "juicio_economico3:nuli", "id1:ins", 
                      "id1:nuli", "ins:nuli", "juicio_economico3:id1:ins", 
                      "juicio_economico3:id1:nuli", "juicio_economico3:ins:nuli",
                      "id1:ins:nuli", "juicio_economico3:id1:ins:nuli"))

coef_interes <- coef_interes %>%
  mutate(
    Estimate = as.numeric(Estimate),
    SE = as.numeric(SE),
    Coef. = as.factor(Coef.)  # Asegurar que Coef. sea un factor
  )


#Renombrar variables
new_labels <- c(
  "juicio_economico3" = "Juicio ecoonómico negativo (β1)",
  "id1" = "Identificación partidista (β2)",
  "ins" = "Institucionalización (β3)",
  "nuli" = "Liderazgo carismático (β8)",
  "juicio_economico3:id1" = "Interacción: Juicio económico negativo e identificación partidista (β4)",
  "juicio_economico3:ins" = "Interacción: Juicio económico negativo e institucionalización (β5)",
  "juicio_economico3:nuli" = "Interacción: Juicio económico negativo y liderazgo carismático (β9)",
  "id1:ins" = "Interacción: Identificación partidista e institucionalización (β6)",
  "id1:nuli" = "Interacción: Identificación partidista y liderazgo carismático (β10)",
  "ins:nuli" = "Interacción: Institucionalización y liderazgo carismático (β11)",
  "juicio_economico3:id1:ins" = "Interacción: Juicio económico negativo, identificación e institucionalización (β7)",
  "juicio_economico3:id1:nuli" = "Interacción: Juicio económico negativo, identificación y liderazgo (β12)",
  "juicio_economico3:ins:nuli" = "Interacción: Juicio económico negativo, institucionalización y liderazgo (β13)",
  "id1:ins:nuli" = "Interacción: Identificación partidista, institucionalización y liderazgo  (β14)",
  "juicio_economico3:id1:ins:nuli" = "Interacción: Juicio económico negativo, identificación, institucionalización y liderazgo (β15)"
)

#CREAR GRÁFICO DE COEFICIENTES

grafica <- ggplot(coef_interes, aes(x = Estimate, y = Coef.)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Estimate - 1.96 * SE, xmax = Estimate + 1.96 * SE), height = 0.2) +
  theme_minimal() +
  labs(x = "Estimación del Coeficiente",
       y = "Variables") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_y_discrete(labels = new_labels)





