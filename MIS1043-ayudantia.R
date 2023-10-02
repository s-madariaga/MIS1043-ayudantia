
# Borrar todos los objetos, ¿Cómo se realizaba esta acción?


# Si no tiene instalado algunos de los paquetes que estána  continuación, instale las instrucciones:
install.packages("tidyverse")
install.packages("haven")
install.packages("knitr")
install.packages("psych")

# Improtar paquetes
library(tidyverse)
library(haven)
library(psych)
library(knitr)

# Ejercicio 1

# A continuación, usted trabajará en la base de datos del Estudio Longitudinal Social de Chile 2016-2022.

#* region
#* comuna
#* annio
#* tendencia
#* confianza_gobierno


# Importe la base de datos

elsoc <- read_dta("data/elsoc_2016-2022.dta")
head(elsoc)

# 1) Seleccione las variables de interés

# 2) Estadísicos descriptivos de confianza gobierno: promedio, desviación estnadar, mediana, mínimo y máximo

# 3) Promedio y desviación estándar de la variable

# 4) Reporte el promedio y desviación estándar de la variable confianza_gobierno, agrupando
# por la variable anio e tendencia.


# Ejercicio 2

# 1) Elabore un histograma de la variable edad.


# 2) Elabore un gráfico de barras con el porcentaje de encuestados ‘tendencia“.
# Luego, agrupe los datos según los años.

# Gráfico 1

# Gráfico 2

# BONUS: Añada porcentaje

# 3) Elabore una gráfica del tiempo de la comuna durante el año 2016. Seleccione solo las primeras 100 observaciones.


# 4) De gran interés es observar cómo ha variado la confianza en el gobierno a lo largo de los años.
# Elabore un gráfico de líneas del promedio de confianza_gobierno por anio. Luego, genere la
# mejor visualización para agrupar estos promedios por tendencia.


# Aplique los siguientes ajustes al gráfico anterior:
# - Aplique el tema bw a su gráfico.
# - Cambie el tamaño de la letra.
# - Añada el título: “Confianza en el gobierno”.
# - Añada una etiqueta correcta de los ejes x e y.
# - Añada una leyenda: “Fuente: Elaboración propia a partir de los datos de ELSOC 2016-2022”.


# Guardar el gráfico
ggsave(filename = "output/grafico_longitudinal.png",
       width = 7,
       height = 4,
       units = 'in',
       dpi = 300)

# Ejercicio 5 (clase 5)



# 1. Obtenga el porcentaje por columna, según corresponda al tipo de variable y nivel de
# medición.
# 2. Obtenga la media y la mediana, según corresponda al tipo de variable y nivel de medición.
# 3. EXTRA: Obtenga una tabla de 2 vías, según corresponda al tipo de variable y nivel de
# medición.


# Ejercicio 6 (clase 6)

#https://deis.minsal.cl/#tableros
#notese, que no escribimos con ñ por notación
data_df <- data.frame(
  ANIO = c(2018, 2018, 2018, 2019, 2019, 2019, 2020, 2020, 2020, 2021, 2021, 2021, 2022, 2022, 2022, 2023, 2023, 2023),
  Frecuencia = c(262, 346, 124, 267, 414, 137, 160, 348, 154, 250, 442, 130, 254, 368, 209, 103, 162, 142),
  CAUSAL = c("Causal 1", "Causal 2", "Causal 3", "Causal 1", "Causal 2", "Causal 3", "Causal 1", "Causal 2", "Causal 3",
             "Causal 1", "Causal 2", "Causal 3", "Causal 1", "Causal 2", "Causal 3", "Causal 1", "Causal 2", "Causal 3")
)
#Ejemplo de un gráfico con el total, sin división en causales
data_df %>% 
  group_by(ANIO) %>% 
  summarise(total=sum(Frecuencia, na.rm=T)) %>% 
  ggplot(aes(ANIO, total, group=1))+
  geom_point()+
  theme_minimal()

