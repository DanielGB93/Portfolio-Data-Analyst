#=======================================
#Diplomado: Estadistica Aplicada
#Nombre: Daniel Garcia
#Fecha: 19/11/2022
#Tema: Logit
#=======================================

#=========
#Librerias
#=========
library(dplyr)
library(ggplot2)
library(stargazer)

#=================
#1.Area de trabajo
#=================

setwd("C:/Users/pc/Documents/Cursos/Estadística Aplicada- GEM/Modelamiento estadístico/Sesion09")
getwd()

#==========================================
#Tratamiento de datos
#==========================================

datos <- read.csv("datos.csv")
str(datos)

base <-  datos %>% 
  select(clasificador = Status, antiguedad= Seniority, vivienda= Home, 
         edad= Age,civil=Marital, trabajo= Job, gasto= Expenses, ingreso= Income,
         deuda= Debt, cantidad= Amount, ahorro= Savings) %>%
  #recodificacion y conversion a factor
  mutate(clasificador= if_else(clasificador== "good",1,0)) %>%#recodificacion y conversion a factor
  mutate(clasificador=factor(clasificador, levels=c(0,1), labels=c("Mal pagador", "Buen pagador")),
         vivienda= as.factor(vivienda),civil=as.factor(civil), 
         trabajo= as.factor(trabajo))
str(base)

#====================
#Análisis descriptivo
#====================
install.packages("descr")
library(descr) 
crosstab(base$clasificador, base$trabajo, prop.r = T, dnn= c("Clasificador", "tipo de trabajo"))
crosstab(base$clasificador, base$trabajo, prop.c = T, dnn= c("Clasificador", "tipo de trabajo"))

#===================
#Análisis gráfico
#===================
base %>%
  ggplot(aes(x=clasificador, y=ingreso))+
  geom_boxplot(aes(color=clasificador), outlier.colour = "blue",
               outlier.size = 3)+
  scale_color_manual(values=c("#c70039", "#0F2BF9"))+
  theme_minimal()+
  labs(title= "Gráfico de cajas de salarios segun tipo de clasificador", caption="Elaboracion propia")+
  coord_flip()

#=========================
#Modelo Estadístico: Logit
#=========================

#Cambiando la base de la variable estado civil 
base <- base %>%
  mutate(civil=relevel(civil, ref= "married"))

options(scipen=999) #para apagar la notacion cientifica
logit <- glm(clasificador ~ antiguedad + vivienda+ edad+ civil +trabajo + gasto 
             + ingreso + deuda+ cantidad+ ahorro, data= base, family= binomial(link = "logit"))
summary(logit)
#Como estrategia, debemos definir la catergoria base como la que tiene mayor 
#robustes de datos, es decir la que tiene más datos. En el caso de estado civil
# está tomando como base divorciado que solo tiene 38 


         