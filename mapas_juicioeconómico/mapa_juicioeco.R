###MAPAS SOBRE  JUICIO ECONÓMICO 
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(sf)
library(haven)
library(paletteer)
library(RColorBrewer)

library(patchwork)



#MAPA AMÉRICA LATINA

mapa_america <- st_read("america_latina.shp")

print(mapa_america)

ggplot(data = mapa_america) +
  geom_sf(fill = "white", color = "black") +  # Polígonos con relleno blanco y límites negros
  theme_void()


#Filtrando la vairbale de juicio económico

america<- read.csv("base_completa1.csv")
juicio<-america[c("Año","pais", "juicio_economico")]


datos <- juicio %>%
  mutate(
    Año = as.integer(Año),
    pais = as.factor(pais),
    juicio_economico= replace_na(as.numeric(juicio_economico), 0) #manejo de NAs
  )


# Filtrando la variable juicio económico por cateogrías
datos <- datos %>%
  mutate(
    Año = as.integer(Año),
    pais = as.factor(pais),
    juicio_economico = factor(juicio_economico, levels = c(1, 2, 3), labels = c("Mejor", "Igual", "Peor"))
  )




# Contar las frecuencias de cada categoría por país y año
datos_agregados <- datos %>%
  group_by(pais, Año, juicio_economico) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(pais, Año) %>%
  mutate(proporcion = n / sum(n))  # Calcula la proporción de cada categoría dentro del grupo

# 'datos_agregados' ahora contiene las proporciones de 'Mejor', 'Igual' y 'Peor' para cada país y cada año.


#HACER COMPATIBLE LOS NOMBRES DE LOS PAÍSES DE DE LA BASE DEL MAPA Y DE  JUICIO ECONÓMICO 
mapa_con_datos <- left_join(mapa_america, datos_agregados, by = c("PAÍS" = "pais"))
mapa_america <- mapa_america %>%
  mutate(PAÍS = case_when(
    PAÍS == "Brasil" ~ "Brazil",
    PAÍS == "El Salvador" ~ "El_Salvador",
    TRUE ~ PAÍS  
  ))


ggplot(data = mapa_con_datos) +
  geom_sf(fill = "white", color = "black") +  # Polígonos con relleno blanco y límites negros
  theme_void()



#FILTRADO DE LAS BASES DE DATOS POR AÑOS PARA "MEJOR"

#PARA 2008
datos_2008 <- mapa_con_datos %>%
  filter(Año == 2008, !is.na(proporcion))

# Ahora filtrar para la categoría "Mejor" y que la proporción no sea NA
datos_mejor08 <- datos_2008 %>%
  filter(juicio_economico == "Mejor", !is.na(proporcion))

#PARA 2010
datos_2010 <- mapa_con_datos %>%
  filter(Año == 2010, !is.na(proporcion))

# Ahora filtrar para la categoría "Mejor" y que la proporción no sea NA
datos_mejor10 <- datos_2010 %>%
  filter(juicio_economico == "Mejor", !is.na(proporcion))


#PARA 2012
datos_2012 <- mapa_con_datos %>%
  filter(Año == 2012, !is.na(proporcion))

# Ahora filtrar para la categoría "Mejor" y que la proporción no sea NA
datos_mejor12 <- datos_2012 %>%
  filter(juicio_economico == "Mejor", !is.na(proporcion))


#PARA 2014
datos_2014 <- mapa_con_datos %>%
  filter(Año == 2014, !is.na(proporcion))

# Ahora filtrar para la categoría "Mejor" y que la proporción no sea NA
datos_mejor14 <- datos_2014 %>%
  filter(juicio_economico == "Mejor", !is.na(proporcion))


#PARA 2016
datos_2016 <- mapa_con_datos %>%
  filter(Año == 2016, !is.na(proporcion))

# Ahora filtrar para la categoría "Mejor" y que la proporción no sea NA
datos_mejor16 <- datos_2016 %>%
  filter(juicio_economico == "Mejor", !is.na(proporcion))



#PARA 2018
datos_2018 <- mapa_con_datos %>%
  filter(Año == 2018, !is.na(proporcion))

# Ahora filtrar para la categoría "Mejor" y que la proporción no sea NA
datos_mejor18 <- datos_2018 %>%
  filter(juicio_economico == "Mejor", !is.na(proporcion))

datos_completosmejor <- rbind(datos_mejor08, datos_mejor10, datos_mejor12, datos_mejor14, datos_mejor16, datos_mejor18)

datos_completosmejor$geometry <- NULL


# Guardar los datos_completos como archivo CSV
write.csv(datos_completosmejor, "datos_completosmejor.csv", row.names = FALSE)





 
#MAPEO DE LA VARIABLE "MEJOR"
colores_divergentes <- c("Mejor" = "green", "Igual" = "gold", "Peor" = "red")


# PARA 2008

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Mejor", sobre el mapa base.
mapa_mejor08 <- mapa_base +
  geom_sf(data = datos_2008 %>% filter(juicio_economico == "Mejor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2008", fill = "Proporción de la respuesta Mejor")

# Imprimir el mapa
print(mapa_mejor08)

#PARA 2010


# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Mejor", sobre el mapa base.
mapa_mejor10 <- mapa_base +
  geom_sf(data = datos_2010 %>% filter(juicio_economico == "Mejor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2010", fill = "Proporción de la respuesta Mejor")

# Imprimir el mapa
print(mapa_mejor10)


#PARA 2012

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Mejor", sobre el mapa base.
mapa_mejor12 <- mapa_base +
  geom_sf(data = datos_2012 %>% filter(juicio_economico == "Mejor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2012", fill = "Proporción de la respuesta Mejor")

# Imprimir el mapa
print(mapa_mejor12)



#PARA 2014

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Mejor", sobre el mapa base.
mapa_mejor14 <- mapa_base +
  geom_sf(data = datos_2014 %>% filter(juicio_economico == "Mejor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2014", fill = "Proporción de la respuesta Mejor")

# Imprimir el mapa
print(mapa_mejor14)


#PARA 2016

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Mejor", sobre el mapa base.
mapa_mejor16 <- mapa_base +
  geom_sf(data = datos_2016 %>% filter(juicio_economico == "Mejor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2016", fill = "Proporción de la respuesta Mejor")

# Imprimir el mapa
print(mapa_mejor16)

#PARA 2018

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Mejor", sobre el mapa base.
mapa_mejor18 <- mapa_base +
  geom_sf(data = datos_2018 %>% filter(juicio_economico == "Mejor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "green", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2018", fill = "Proporción de la respuesta Mejor")

# Imprimir el mapa
print(mapa_mejor18)


#UNIENDO LOS MAPAS EN UNA SOLA IMAGEN

layout_mejor <- (mapa_mejor08 | mapa_mejor10| mapa_mejor12) /
  (mapa_mejor14 | mapa_mejor16 |mapa_mejor18)



barra_colores <- guide_colourbar(title = "Proporción de la respuesta Mejor",
                                 title.position = "top",
                                 direction = "vertical",
                                 order = 1)
layout_mejor <- layout_mejor & theme(legend.position = "right") & guides(fill = barra_colores)

layout_mejor <- layout_mejor + plot_layout(guides = 'collect')

print(layout_mejor)







####PARA LA CATEOGRÍA IGUAL####
#FILTRADO DE LAS BASES DE DATOS


#PARA 2008
datos_2008 <- mapa_con_datos %>%
  filter(Año == 2008, !is.na(proporcion))

# Ahora filtrar para la categoría "Igual" y que la proporción no sea NA
datos_igual08 <- datos_2008 %>%
  filter(juicio_economico == "Igual", !is.na(proporcion))

#PARA 2010
datos_2010 <- mapa_con_datos %>%
  filter(Año == 2010, !is.na(proporcion))

# Ahora filtrar para la categoría "Igual" y que la proporción no sea NA
datos_igual10 <- datos_2010 %>%
  filter(juicio_economico == "Igual", !is.na(proporcion))


#PARA 2012
datos_2012 <- mapa_con_datos %>%
  filter(Año == 2012, !is.na(proporcion))

# Ahora filtrar para la categoría "Igual" y que la proporción no sea NA
datos_igual12 <- datos_2012 %>%
  filter(juicio_economico == "Igual", !is.na(proporcion))


#PARA 2014
datos_2014 <- mapa_con_datos %>%
  filter(Año == 2014, !is.na(proporcion))

# Ahora filtrar para la categoría "Igualr" y que la proporción no sea NA
datos_igual14 <- datos_2014 %>%
  filter(juicio_economico == "Igual", !is.na(proporcion))


#PARA 2016
datos_2016 <- mapa_con_datos %>%
  filter(Año == 2016, !is.na(proporcion))

# Ahora filtrar para la categoría "Igual" y que la proporción no sea NA
datos_igual16 <- datos_2016 %>%
  filter(juicio_economico == "Igual", !is.na(proporcion))



#PARA 2018
datos_2018 <- mapa_con_datos %>%
  filter(Año == 2018, !is.na(proporcion))

# Ahora filtrar para la categoría "Igual" y que la proporción no sea NA
datos_igual18 <- datos_2018 %>%
  filter(juicio_economico == "Igual", !is.na(proporcion))



datos_completosigual <- rbind(datos_igual08, datos_igual10, datos_igual12, datos_igual14, datos_igual16, datos_igual18)

datos_completosigual$geometry <- NULL


# Guardar los datos_completos como archivo CSV
write.csv(datos_completosigual, "datos_completosigual.csv", row.names = FALSE)






#MAPEO DE LA VARIABLE IGUAl
colores_divergentes <- c("Mejor" = "green", "Igual" = "gold", "Peor" = "red")

# PARA 2008

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Igual", sobre el mapa base.
mapa_igual08 <- mapa_base +
  geom_sf(data = datos_2008 %>% filter(juicio_economico == "Igual"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "gold", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2008", fill = "Proporción de la respuesta Igual")

# Imprimir el mapa
print(mapa_igual08)

#PARA 2010


# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Igual", sobre el mapa base.
mapa_igual10 <- mapa_base +
  geom_sf(data = datos_2010 %>% filter(juicio_economico == "Igual"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "gold", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2010", fill = "Proporción de la respuesta Igual")

# Imprimir el mapa
print(mapa_igual10)


#PARA 2012

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Igual", sobre el mapa base.
mapa_igual12 <- mapa_base +
  geom_sf(data = datos_2012 %>% filter(juicio_economico == "Igual"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "gold", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2012", fill = "Proporción de la respuesta Igual")

# Imprimir el mapa
print(mapa_igual12)



#PARA 2014

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Igual", sobre el mapa base.
mapa_igual14 <- mapa_base +
  geom_sf(data = datos_2014 %>% filter(juicio_economico == "Igual"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "gold", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2014", fill = "Proporción de la respuesta Igual")

# Imprimir el mapa
print(mapa_igual14)


#PARA 2016

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Igual", sobre el mapa base.
mapa_igual16 <- mapa_base +
  geom_sf(data = datos_2016 %>% filter(juicio_economico == "Igual"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "gold", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2016", fill = "Proporción de la respuesta Igual")

# Imprimir el mapa
print(mapa_igual16)

#PARA 2018

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Igual", sobre el mapa base.
mapa_igual18 <- mapa_base +
  geom_sf(data = datos_2018 %>% filter(juicio_economico == "Igual"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "gold", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2018", fill = "Proporción de la respuesta Igual")

# Imprimir el mapa
print(mapa_igual18)


#Uniendo todos los mapas en una imagen 
layout_igual <- (mapa_igual08 | mapa_igual10| mapa_igual12) /
  (mapa_igual14 | mapa_igual16 |mapa_igual18)



barra_colores <- guide_colourbar(title = "Proporción de la respuesta Igual",
                                 title.position = "top",
                                 direction = "vertical",
                                 order = 1)
layout_igual <- layout_igual & theme(legend.position = "right") & guides(fill = barra_colores)

layout_igual <- layout_igual + plot_layout(guides = 'collect')

print(layout_igual)




#######PARA PEOR######



#FILTRADO DE LAS BASES DE DATOS


#PARA 2008
datos_2008 <- mapa_con_datos %>%
  filter(Año == 2008, !is.na(proporcion))

# Ahora filtrar para la categoría "Igual" y que la proporción no sea NA
datos_peor08 <- datos_2008 %>%
  filter(juicio_economico == "Peor", !is.na(proporcion))

#PARA 2010
datos_2010 <- mapa_con_datos %>%
  filter(Año == 2010, !is.na(proporcion))

datos_peor10 <- datos_2010 %>%
  filter(juicio_economico == "Peor", !is.na(proporcion))


#PARA 2012
datos_2012 <- mapa_con_datos %>%
  filter(Año == 2012, !is.na(proporcion))

datos_peor12 <- datos_2012 %>%
  filter(juicio_economico == "Peor", !is.na(proporcion))


#PARA 2014
datos_2014 <- mapa_con_datos %>%
  filter(Año == 2014, !is.na(proporcion))

datos_peor14 <- datos_2014 %>%
  filter(juicio_economico == "Peor", !is.na(proporcion))


#PARA 2016
datos_2016 <- mapa_con_datos %>%
  filter(Año == 2016, !is.na(proporcion))

datos_peor16 <- datos_2016 %>%
  filter(juicio_economico == "Peor", !is.na(proporcion))



#PARA 2018
datos_2018 <- mapa_con_datos %>%
  filter(Año == 2018, !is.na(proporcion))

datos_peor18 <- datos_2018 %>%
  filter(juicio_economico == "Peor", !is.na(proporcion))


datos_completospeor <- rbind(datos_peor08, datos_peor10, datos_peor12, datos_peor14, datos_peor16, datos_peor18)

datos_completospeor$geometry <- NULL


# Guardar los datos_completos como archivo CSV
write.csv(datos_completospeor, "datos_completospeor.csv", row.names = FALSE)









#MAPEO DE LA VARIABLE PEOR
colores_divergentes <- c("Mejor" = "green", "Igual" = "gold", "Peor" = "red")


# PARA 2008

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Peor", sobre el mapa base.
mapa_peor08 <- mapa_base +
  geom_sf(data = datos_2008 %>% filter(juicio_economico == "Peor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2008")

# Imprimir el mapa
print(mapa_peor08)

#PARA 2010


# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Peor", sobre el mapa base.
mapa_peor10 <- mapa_base +
  geom_sf(data = datos_2010 %>% filter(juicio_economico == "Peor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2010")

# Imprimir el mapa
print(mapa_peor10)


#PARA 2012

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Peor", sobre el mapa base.
mapa_peor12 <- mapa_base +
  geom_sf(data = datos_2012 %>% filter(juicio_economico == "Peor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2012")

# Imprimir el mapa
print(mapa_peor12)



#PARA 2014

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Peor", sobre el mapa base.
mapa_peor14 <- mapa_base +
  geom_sf(data = datos_2014 %>% filter(juicio_economico == "Peor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2014")

# Imprimir el mapa
print(mapa_peor14)


#PARA 2016

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Peor", sobre el mapa base.
mapa_peor16 <- mapa_base +
  geom_sf(data = datos_2016 %>% filter(juicio_economico == "Peor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2016")

# Imprimir el mapa
print(mapa_peor16)

#PARA 2018

# Primero, crea una capa base con todos los países en un color por defecto.
mapa_base <- ggplot(data = mapa_con_datos) +
  geom_sf(fill = "lightgrey", color = "black")  # Colorea todos los países de gris claro.

# Luego, añade otra capa con los datos filtrados para "Peor", sobre el mapa base.
mapa_peor18 <- mapa_base +
  geom_sf(data = datos_2018 %>% filter(juicio_economico == "Peor"), aes(fill = proporcion), color = "black") +
  scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey", limits = c(0, 1)) +
  theme_void() +
  labs(title = "2018")

# Imprimir el mapa
print(mapa_peor18)


#UNIENDO TODOS LOS MAPAS EN UNA IMAGEN 

layout_peor <- (mapa_peor08 | mapa_peor10| mapa_peor12) /
  (mapa_peor14 | mapa_peor16 |mapa_peor18)



barra_colores <- guide_colourbar(title = "Proporción de la respuesta Peor",
                                 title.position = "top",
                                 direction = "vertical",
                                 order = 1)
layout_peor <- layout_peor & theme(legend.position = "right") & guides(fill = barra_colores)

layout_peor <- layout_peor + plot_layout(guides = 'collect')

print(layout_peor)



