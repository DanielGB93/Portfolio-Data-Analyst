#================================================
#Diplomado: Estadistica Aplicada
#Nombre: Dax Mancilla
#Fecha: 22/10/2022
#Tema: Modelos de Regresión Lineal
#===============================================

#=========
#Librerias
#=========
library(dplyr)
library(ggplot2)


#=================
#1.Area de trabajo
#=================
# ejecucion: Ctrl + enter

setwd("D:/OneDrive - INDECOPI/Escritorio/diplomado Estadistica 151022/Sesion 4")
getwd()

library(haven)
enaho <- read_dta("Enaho-recodificada-2019.dta")


#==============================
#2.Tratamiento de base de datos
#==============================

#-----------------------------------------
# Recodificación y selección de variables
#-----------------------------------------
base %>% 
  count(dominio)

#base %>% 
  #count(educacion)

table(as_factor(base$educacion))


base <- enaho %>% 
        select(miembro_hogar=mieperho, gasto=gashog2d, ingreso=inghog2d, dominio,
               sexo=p207, edad=p208a, educacion=p301a) %>% 
        mutate(dominio=case_when(as.numeric(dominio)<=3 | as.numeric(dominio)==8 ~ "Costa",
                                 as.numeric(dominio)<=6 ~ "Sierra", TRUE ~ "Selva"),
               educacion=case_when(as.numeric(educacion)<=4 |as.numeric(educacion)==12  ~ "Primaria",
                                   as.numeric(educacion)<=6 ~ "Secundaria", TRUE ~ "Superior"),
               sexo=if_else(sexo==1, "Hombre","Mujer"),
               gasto=gasto/12,
               ingreso=ingreso/12)
        

#---------------------
#convertimos a factor
#---------------------
base <- base %>% 
  mutate(dominio=as.factor(dominio),
         educacion=as.factor(educacion),
         sexo=as.factor(sexo))


#===================
#2.Analisis grafico
#===================
base %>% 
  ggplot(aes(x=log(ingreso), y=log(gasto))) +
  geom_point(aes(color=sexo), alpha=0.5) +
  geom_smooth(method= "lm", color="red") +
  facet_wrap(~sexo) + 
  labs(title="ingreso y gasto segun sexo", caption= "Elaboracion propia")


#grafico de cajas
#----------------
base %>% 
  slice_sample(n=5000) %>% 
  ggplot(aes(x=dominio, y=gasto))+
  geom_boxplot(aes(fill=dominio), outlier.shape = 1, outlier.color = "blue", outlier.size = 3) +
  coord_flip()+
  geom_jitter(alpha=0.5, width = 0.2)+
  theme_minimal()

#=============================
#3.Analisis  del modelo lineal
#=============================
options(scipen=999)

#Ho: b0 = b1 = b2 = ... = 0 (no es estadisticamente significativos)
#H1: b0 = b1 = b2 = ... != 0 (son estadisticamente significativos)

#p-valor <=0.05 entonces rechazamos la Ho

#Modelo 1:
#---------
reg1 <- lm(gasto ~ ingreso + edad, data=base)
summary(reg1)

#Modelo 2:
#---------
reg2 <- lm(gasto ~ ingreso + edad +  dominio, data=base)
summary(reg2)

#Modelo 3:
#---------
reg3 <- lm(gasto ~ ingreso + edad +  dominio + educacion, data=base)
summary(reg3)


#Comparacion de modelos
#----------------------
#install.packages("stargazer")
library(stargazer)

stargazer(reg1, reg2, reg3, type="text", title="Modelos de regresión lineal",
          column.labels =  c("Modelo 1", "Modelo 2", "Modelo 3"))


#Analisis de criterio de información
#-----------------------------------
AIC(reg1, reg2, reg3)
BIC(reg1, reg2, reg3)

#========================================
#4. Post Estimación (test de diagnostico)
#========================================

#Prueba de multicolinealidad
#---------------------------
#install.packages("car")
library(car)
vif(reg3)

#Prueba de Heterocedasticidad
#----------------------------

#Ho: Homocedasticidad  ; H1: Heterocedasticidad


#Grafico
#-------
residuos <- resid(reg3)
obs <- c(1:121623)

plot(obs, residuos, type="l")


#Test de Golffed - Quant
#-----------------------
install.packages("lmtest")
library(lmtest)
gqtest(reg3)

#Test de Breush -Pagan
#---------------------
bptest(reg3)


#===================================
#5. Correción de heterocedasticidad
#===================================

#Minimo cuadrados ponderado (WLS)
#--------------------------------

#Modelo auxiliar para capturar unna proxy de la varianza

aux <- lm(residuos ~ reg3$fitted.values)

wt <- 1 / ((aux$fitted.values)^2)


reg3_mcp <- lm(gasto ~ ingreso + edad + dominio + educacion, data=base, weights = wt)

summary(wt==Inf)

wt

base_wt <- base %>% 
          mutate(wt=wt) %>% 
          filter(wt!=Inf)

reg3_mcp <- lm(gasto ~ ingreso + edad + dominio + educacion, data=base_wt, weights = wt)
summary(reg3_mcp)

stargazer(reg3, reg3_mcp, type="text", title="Corrección de heterocedasticidad")

