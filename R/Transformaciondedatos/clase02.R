#===================================
#Diplomado: Estadistica Aplicada
#Nombre: Daniel Garc?a
#Fecha: 15/10/2022
#Tema: Introducci?n a herramientas b?sicas de R y manipulaci?n de bases de 
#datos
#===================================

#1. Area de trabajo

setwd("C:/Users/pc/Documents/Cursos/Estad?stica Aplicada- GEM/Modelamiento 
      #estad?stico/Sesi?n 02")
getwd()

#2. Instalaci?n de paquetes: install.packages("nombre_del_paquete") ? en la 
#pesta?a "packages"

install.packages("ggplot2") #Paso 1: Instalaci?n
library(ggplot2) #Paso2: Activaci?n
library(help= ggplot2) #Verificar si est? instalada

#3. Importaci?n de bases de datos
#El comando read.table() es muy general, por lo que hay que especificarle 
#varios par?metros

#3.1 Importar un archivo de texto
bandas_criminales <- read.table("bandas_cri.txt",header= TRUE, sep="\t") #Et?
#separado por tabulaci?n 

#3.2 Importar un archivo csv
bandas_criminales <- read.csv("bandas_cri.csv")

#3.3 Importar un excel
library(readxl) #este paquete ya est? en R. S?lo debemos activarlo
auto_excel <- read_excel("auto.xlsx")

#3.4 Importar Archivo .dta (archivos de stata)
library(haven) #activar previo a leer el archivo .dta
auto_dta <- read_dta("auto.dta")

#3.5 Importar archivos spss
library(haven)
accidentes_sav <-  read_spss("Accidents.sav")

#...me qued? atr?s en esta parte

#Exportacion de bases de datos
install.packages("writexl")
library(writexl)

write_xlsx(accidentes_sav, "accidentes_final.xlsx")


#========================================================================

#===================================================
# 4. Manipulaci?n y transformaci?n de bases de datos
#===================================================
install.packages("dplyr")
library(dplyr)

#Cuando un comando se repite en diferentes librer?as debemos especificar de qu?
#librer?a deseamos extraer el comando usando funcion::paquete

library(help= dplyr)

#base de datos
#-------------
install.packages("ggplot2movies")
library(ggplot2movies)

base <- movies

#dplyr permite conectar funciones en un s?lo comando.

#1. Comando select

base2 <- select(base,pelicula= title, a?o= year, duracion=length, rating, votes)
?select

base3 <- select(base, starts_with("r"))
base4 <- select(base, contains("m"))
base5 <- select(base, 1:5, Comedy, Drama)
base6 <- select(base, -mpaa) #mantiene todas las columnas menos la que le indico

#comando filter (se emplean a nivel de filas)

base7 <- filter(base, year== 2002)
base8 <- filter(base, year==2002 & rating> 8)
base9 <- filter(base, year==2002 | rating >8)
base10 <- filter(base, year>= 2000)
base11 <- filter(base,year %in% c(1994,2000,2005) ) #pasamos un vector como condici?n
base12 <- filter(base, between(year, 1990, 2000)) #rango
base13 <-  filter(base, year != 2003)

#Comando mutate (agregar variables)

base14 <- mutate(base, ratio= rating / votes)
#case_when construye variables categ?ricas
base15 <- mutate(base, clasificacion=case_when(rating<=3 ~"Mala", 
                                               rating<=6 ~"Regular", 
                                               TRUE ~ "Buenas"))

#Comando summarise (calculo estad?stico)
summarise(base, promedio= mean(rating))

summarise(base, mediana= median(rating))


#combinacion de funciones (%>%)

basefinal <- base %>% 
  select(a?o=year, duracion= length, rating, votes) %>%
  filter (a?o ==2005)%>%
  mutate (ratio = rating*votes,
          rating_level= if_else( rating>6, "buena",
                                 if_else(rating<= 6 & rating> 3, "normal", "malo")))
