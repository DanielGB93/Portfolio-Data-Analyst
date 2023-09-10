#================================================
#Diagnostico del Modelo de Regresion Lineal
#================================================

#=========
#Librerias
#=========
library(dplyr)
library(ggplot2)


#=================
#1.Area de trabajo
#=================

setwd("C:/Users/pc/Documents/Cursos/Estadística Aplicada- GEM/Modelamiento estadístico/Sesion5")
getwd()

library(readxl)
datos <- read_excel("rendimiento academico universidad.xlsx")

#====================================
#2.Transformaci?n de la base de datos
#====================================


datos <- datos %>% 
          mutate(Sexo=factor(Sexo, levels=c(0,1), labels=c("Mujer","Hombre")),
                              Niv_educ_mad=factor(Niv_educ_mad,levels=c(0:4),
                                                  labels=c("Sin nivel","Primaria",
                                                           "Secundaria","Tecnico","Universitario")),
                              Niv_educ_pad=factor(Niv_educ_pad,levels=c(0:4),
                                                  labels=c("Sin nivel","Primaria",
                                                           "Secundaria","Tecnico","Universitario")))

#========================
#3.Estimaci?n del modelo
#========================
options(scipen=999)
reg1 <- lm(data=datos, Prom_pond ~ Sexo + Edad + Trab + Hor_estud_sem -1)
summary(reg1)

reg2 <- lm(data=datos, Prom_pond ~ Sexo + Edad + Trab + Hor_estud_sem + Ing_fam +
              Tip_Educ + Uso_comp + Uso_biblio + Comp_doc -1) 
summary(reg2)


#========================================
#4.Supuesto de normalidad de los residuos
#========================================

#Ho: los residuos siguen una distribuci?n normal
#H1: los residuos no siguen una distribuci?n normal

#--------------------------
#Grafico Q -Q de normalidad
#--------------------------
qqnorm(resid(reg2))
plot(reg2,2)

ggplot(reg2, aes(sample =reg2$residuals)) +
    stat_qq() +
    stat_qq_line(colour="red", size=1) +
  theme_bw() +
  labs(x= "Quantiles teoricos", y="Residuos estandarizados")


#--------------------
#Pruebas estadisticas
#--------------------

shapiro.test(reg2$residuals)

#install.packages("nortest")
library(lmtest)
library(nortest)
library(help="nortest")

ad.test(reg2$residuals)
cvm.test(reg2$residuals)
lillie.test(reg2$residuals) 

#Heterocedasticidad
#------------------
bptest(reg2)
gqtest(reg2)


