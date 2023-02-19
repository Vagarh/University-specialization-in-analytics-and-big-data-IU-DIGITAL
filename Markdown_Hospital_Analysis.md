---
title: "Caracterización Población Atendida Hospital Manuel Uribe Angel"
subtitle: "Información sociodemográfica de la población atendida en la E.S.E. Hospital Manuel Uribe Angel del municipio de Envigado durante el año 2020"
author: "Juan Felipe Cardona Arango"
Date: 17 / 02/ 2023
output:
  word_document: default
  pdf_document: default
  html_document:
    df_print: paged
---

------------------------------------------------------------------------

::: text-justify
La caracterización poblacional consiste en un análisis de índole esencialmente descriptivo, puede realizarse desde distintos enfoques metodológicos, haciendo uso de fuentes primarias y secundarias de información, con el propósito de aproximarse al conocimiento y comprensión del tamaño, estructura, características, dinámicas, y experiencias asociadas a la población migrantes, refugiada y retornada en un momento del tiempo específico.
:::

::: text-justify
Por consiguiente, se han definido las siguientes sublíneas para su análisis:

1.  Sexo

2.  Edad

3.  Curso de vida

4.  Pertenencia étnica Grupos poblacionales

5.  Ubicación geográfica

6.  País de procedencia

7.  Nacimientos Defunciones

8.  Ocupación

9.  Educación

10. Estatus familiar

11. ingresos

Para este ejemplo de naturaleza descriptiva se usara la fuente de datos disponible en [Datos abiertos GovCol](https://www.datos.gov.co/Estad-sticas-Nacionales/Caracterizaci-n-poblaci-n-atendida-Hospital-Manuel/4ike-xz34)

El primer paso consistio en cargar el archivo csv en el IDE, Rstudio
:::

```{r}
library(tidyverse)# cargar liberia tidyverse el cual es con conjuto varias liberias de analisis de datos 
install.packages("modeest")
library(modeest)
db <- read_csv("~/Downloads/Caracterizaci_n_poblaci_n_atendida_Hospital_Manuel_Uribe_Angel.csv")
```

::: text-justify
Se inicia el proceso exploratorio de datos, con el fin de compreder las variables, detectar datos atipicos , examinar patrones o relaciones
:::

```{r}
head(base_datos,n=10) # previsualizo las primeras 10 lineas, se revisa coherencia del tipo de datos  chr, la variable edad a dbl
tail(base_datos,n=10) # previsualizo las ultimas 10 lineas
sum(is.na(base_datos))# se explora si existen datos faltanse con el temrino NA
```

::: text-justify
Se define una funcion reutilizable que permite ejecutar las medidas de tendencia central en cada una de las columnas .
:::

```{r}
medidas_tendecia <- function(x){
  w <- mean(x,na.rm = FALSE) # media
  e <- median(x,na.rm = FALSE) # mediana
  j <- min(x) # minimo
  l <- max(x) # maximo
  c <- mfv(x) # moda 
  print(w)
  print(e)
  print(j)
  print(l)
  print(c)
}
# defino mi funcion que tome un elemento del data frame como lista y realize los calculos de mis medidas de tendencia central medi, moda y mediana
```

::: text-justify
Se selecciona las variables edad y sexo como con el fin de agrupar patrones de tendencia de consultas a la instucion.

Se implementa la libreria ggplto2 para la creacion de 2 tipos de graficos Histrogramas de frecuencia y Diagramas de cajas para observar la distirbucion de los datos
:::

::: text-justify
Diagrama de cajas : Es un método estandarizado para representar gráficamente una serie de datos numéricos a través de sus [cuartiles](https://es.wikipedia.org/wiki/Cuartiles "Cuartiles"). De esta manera, se muestran a simple vista la [mediana](https://es.wikipedia.org/wiki/Mediana "Mediana") y los cuartiles de los datos,^[1](https://es.wikipedia.org/wiki/Diagrama_de_caja#cite_note-1)^​ y también pueden representarse sus [valores atípicos](https://es.wikipedia.org/wiki/Valor_at%C3%ADpico "Valor atípico"). :::
:::

::: text-justify
Historgramas: Es una representación [gráfica](https://es.wikipedia.org/wiki/Gr%C3%A1fica "Gráfica") de una [variable](https://es.wikipedia.org/wiki/Variable_estad%C3%ADstica "Variable estadística") en forma de barras, donde la superficie de cada barra es proporcional a la [frecuencia](https://es.wikipedia.org/wiki/Frecuencia "Frecuencia") de los valores representados. Sirven para obtener una "primera vista" general, o panorama, de la distribución de la población, o de la muestra, respecto a una característica, cuantitativa y continua
:::

-   Grafica 1

```{r}
## grafico 1 histograma  de Zona de residencia y sumatoria de pacientes
ggplot(data = base_datos) +
  geom_bar(aes(x = ZONA_RES),
           width=0.2,
           fill='tomato2') +
  labs(title = "ZONA DE RESIDENCIA",
       x = "ZONA DE RESIDENCIA DE PACIENTE",
       y = "Sum de Usuarios año")+
       coord_flip()
      

```

![](Grafica1-1.png)

-   Grafica 2

```{r}
## grafica 2 box plot edad y sexo
ggplot(data = base_datos) +
  geom_boxplot(aes(x = SEXO, y = EDAD ))+
  labs(title = "Edad vs Sexo",
       x = "Sexo",
       y = "Edad") 
```

![](Grafica2_1.png)

-   Grafica 3

```{r}
## grafica 3 histograma  sexo
ggplot(data = base_datos) +
  geom_bar(aes(x = SEXO),
           width=0.3,
           fill='tomato2') +
  labs(title = "Sexo de la Poblacion Atendida 2020",
       x = "Genero",
       y = "Conteo en el año")

```

![](Grafica3_1.png)

-   Grafica 4

```{r}

ggplot(data = base_datos) +
  geom_bar(aes(x = GRUPO_EDAD),
           width=0.5) +
  labs(title = "Poblacion Atendida 2020 por grupo de Edad",
       x = "Grupo Edad",
       y = "Conteo en el año")+
       coord_flip()
```

--![](Grafica4_1.png)

A partir de los resultados desde un enfoque descriptivo se puede concluir que

-   La mayoria de la poblacion que atiende el centro es de origen Urbano
-   Que hay poca diferencia en las medidas de tedencia central en la edad entre los generos (Grafica 2)
-   El genero femenino es el mayor demandante de atencion en el centro hospitalario (Grafica 3)
-   Que el grupo etario que consulta mas la institucion es el grupo de entre los 25 a 29 años (Grafica 4)

|    Date    | Change Description  | Version |
|:----------:|:-------------------:|:-------:|
| 2023-02-18 | Added lab to GitLab |   1.0   |
| 2023-02-28 |       Update        |   1.1   |
