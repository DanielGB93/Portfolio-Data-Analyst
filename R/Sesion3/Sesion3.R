#Tema: Graficación con ggplot2

#activar/ Instalar librerías

library(ggplot2)
library(dplyr)
library(car)

#cargar base
data("Salaries")
?Salaries

Salaries <- Salaries %>%
  rename(años_servicio= "yrs.service", sexo= "sex", salario= "salary", 
         tipo= "rank")%>%
  mutate(tipo2= factor(tipo, labels=c("Asistentes", "Asociados", "Principal")))

#Capa elemental de configuración de variables

g1 <- ggplot(data=Salaries, aes(x=años_servicio, y= salario)) #Aes tiene que ver con estética

#Capa geometrica
#Gráfico de dispersion
g1 + geom_point()

#Coloreamos por la variable sexo
g1 + geom_point(aes(col= sexo))

#cambiar la forma de los círculos
g1 + geom_point(aes(col=sexo, shape= sexo))

#cambiar el tamaño de la forma
g1 + geom_point(aes(col=sexo, shape= sexo), size= 2.9)

g2 <- g1 + geom_point(aes(col=sexo, shape= sexo), size= 2.9)

#modificación manual de formas y colores

vignette( "ggplot2-specs") # Ver tipos de línea en help
g2 + scale_alpha_manual(values=c(17,18))+
  scale_color_manual(values=c("#CD5C5C", "#F0E68C"))

g3 <- g2 + scale_alpha_manual(values=c(17,18))+
  scale_color_manual(values=c("#CD5C5C", "#F0E68C"))

#Capa agragar titulo 
g3 +labs(title= "Salario por sexo")

#Capa agragar titulo y nombres

g3 +labs(title= "Salario por sexo", X= "AÑOS DE SERVICIOS", Y="SALARIOS")
