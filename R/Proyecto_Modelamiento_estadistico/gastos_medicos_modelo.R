#=======================================
#Diplomado: Estadistica Aplicada
#Nombre: Daniel García
#Fecha: 06/12/2022
#Proyecto cálculo de gastos médicos
#=======================================

library(dplyr)
library(ggplot2)
library(olsrr)

setwd("C:/Users/pc/Documents/Cursos/Estadística Aplicada- GEM/Modelamiento estadístico/Proyecto_Modelamiento_estadistico")
getwd()

data <- read.csv("insurance.csv")

datos <- data %>% 
  mutate(sex= if_else(sex=="male", 1, 0), smoker= if_else(smoker=="yes", 1, 0))

#Análisis gráfico 
datos%>% 
ggplot(aes(x=bmi, y=charges))+
geom_point(aes(color=smoker))+
labs(title="Gastos médicos facturados e Indice de Masa Muscular", 
     caption= "Elaboracion propia", x="Indice de masa muscular",
     y="Gastos")

#El BMI parece no afectar en el cobro de la prima de seguro, a menos que la persona
#sea un fumador activo
 #Relacion edad - gastos medicos 
datos%>% 
  ggplot(aes(x=age, y=charges))+
  geom_point(aes(color=smoker))+
  labs(title="Gastos médicos facturados por edad", caption= "Elaboracion propia", x="Edad", y="Gastos")
#En todas las edades representadas en la muestra, los gastos médicos incrementan si la persona es fumadora

#gr?fico de cajas
#----------------
data %>% 
  ggplot(aes(x=smoker, y=charges))+
  geom_boxplot(aes(fill=smoker))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title="Gastos médicos facturados de fumadores y no fumadores", 
       caption= "Elaboracion propia", x="Fumador",
       y="Gastos")
#Existe una diferencia significativa entre las medias de gastos médicos de las personas que fuman
#y las que no fuman

data %>% 
  ggplot(aes(x=sex, y=charges))+
  geom_boxplot(aes(fill=sex))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title="Gastos médicos por sexo")


#Dataset no fumadores
datos2  %>%
  ggplot(aes(x=bmi, y= charges))+
  geom_point()

#Elaboración del modelo
#Modelo general 
reg1 <- lm(charges ~ bmi + sex+ smoker + age + children, data=datos)
summary(reg1)

#Regresion para los fumadores
#Dataset fumadores
datos2 <- datos %>%
  filter(smoker=="1")

reg2 <- lm(charges ~  bmi + sex+ smoker + age + children, data=datos2)
summary(reg2)

#Regresion para los no fumadores
datos3 <- datos %>%
  filter(smoker=="0")

datos3 %>%
  ggplot(aes(x=bmi, y= charges))+
  geom_point()

reg3 <-  lm(charges ~  bmi + sex+ smoker + age + children, data=datos3)
summary(reg3)
