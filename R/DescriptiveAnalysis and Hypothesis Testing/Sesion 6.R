#=======================================
#Diplomado: Estadistica Aplicada
#Nombre: Daniel Garcia
#Fecha: 29/10/2022
#Tema: Analisis confirmatorio de datos
#=======================================

#=========
#Librerias
#=========
library(dplyr)
library(ggplot2)
install.packages("stringr")
library(stringr)
library(help= "stringr")
#=================
#1.Area de trabajo
#=================

setwd("C:/Users/pc/Documents/Cursos/Estad?stica Aplicada- GEM/Modelamiento estad?stico/Sesion06")
getwd()

#==========================================
#2.Manipulaci?n y transformaci?n de la BBDD
#==========================================
base <- read.csv("fast_food.csv")

Burguer <- base %>% 
          filter(str_detect(item,"[B|b]urger")) #Columna item contenga Burger con B mayuscula o minuscula

Sandwich <- base %>% 
            filter(str_detect(item,"[S|s]andwich")) #Columna item contenga sandwich con S mayuscula o minuscula


#==================================
#3. Analisis descriptivo y gr?fico
#==================================

#Estadisticos
#------------
base %>% 
  group_by(restaurant) %>% 
  summarise(promedio_calorias=mean(calories),
            promedio_cholesterol=mean(cholesterol))

Burguer %>% 
  group_by(restaurant) %>% 
  summarise(promedio_calorias=mean(calories),
            promedio_cholesterol=mean(cholesterol))

Sandwich %>% 
  group_by(restaurant) %>% 
  summarise(promedio_calorias=mean(calories),
            promedio_cholesterol=mean(cholesterol))


#Graficos
#--------
ggplot(base, aes(x=restaurant, y=calories)) +
  geom_boxplot(aes(color=restaurant))


ggplot(Burguer, aes(x=restaurant, y=calories)) +
  geom_boxplot(aes(color=restaurant))


ggplot(Burguer, aes(x=calories)) +
  geom_histogram(aes(fill=restaurant)) +
  facet_grid(.~restaurant)

ggplot(Sandwich, aes(x= calories, y=calories))+
  geom_boxplot(aes(color=restaurant))


#==============================
#4. Hipotesis una sola muestra
#==============================
#Pregunta de hipotesis
#---------------------
mean(Burguer$calories)

#El promedio de caloria por restaurant es diferente a 600

#Ho: u=600
#H1: u!=600

t.test(Burguer$calories, mu=600, alternative="two.sided") #greater , less


#Conclusion: No se rechaza Ho, por lo tanto no hay suficiente evidencia estadistica para
#indicar que el promedio de las calorias es diferente a 600.















