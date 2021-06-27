####
library(ggplot2)
library(tidyverse)
library(stringr)
require(MASS)
require(reshape2)

######################### carga y preparacion de datos ########## 

# Carga datos 
Signal20_21 <- read.csv("~/dev/R/EncuestaSignal/Formulario de satisfaccion 2020-21 para estudiantes.csv", encoding="UTF-8", stringsAsFactors=TRUE)
str(Signal20_21)

# Ponemos nombres más utilizables a las columnas 
colnames(Signal20_21) <-
c("Marca.temporal", "Genero", "ContextoUso", 
  "Aumenta.la.participacion", 
  "Permite.debate", 
  "Facilita.la.comunicación.docente", 
  "Mejora.resolver.dudas", 
  "Ayuda.planificar.actividades", 
  "Comparacion.fácil.de.usar", 
  "Comparacion.motivante", 
  "Comparacion.amigable", 
  "Comparacion.comodo", 
  "Comparcion.util", 
  "Comparacion.informado", 
  "Comparacion.organizardo", 
  "Comparacion.recibir.novedades", 
  "Comparacion.compartir.dudas", 
  "Comparacion.trato.cercano..docente", 
  "Otras ventajas", 
  "Inconvenientes", 
  "Utilizar.Signal.En.Otras.Asignaturas", 
  "Informacion.adecuada.Instalacion", 
  "Informacion.adecuada.Funciones", 
  "Informacion.adecuada.Motivos", 
  "Informacion.adecuada.Privacidad", 
  "Informacion.adecuada.Financiacion", 
  "Funciones.Usadas", 
  "Funciones.Faltantes", 
  "Satisfacion.General.Signal"
)

# Primera columna como fecha 
Signal20_21[["Marca.temporal"]]<-as.Date(Signal20_21[["Marca.temporal"]], format = "%Y/%m/%d %H:%M:%S")
# 11 columnas que ya estan como factores les definimos el orden 
for (i in 4:18){
 Signal20_21[[i]]<-factor(Signal20_21[[i]],
       levels=c("Nada de acuerdo","Algo de acuerdo","Bastante de acuerdo","Muy de acuerdo"))
}
## otras columnas 
for (i in 22:26){
  Signal20_21[[i]]<-factor(Signal20_21[[i]],
                           levels=c("Nada de acuerdo","Algo de acuerdo","Bastante de acuerdo","Muy de acuerdo"))
}

### contabilizar funciones usadas 
funciones <- strsplit(as.character(Signal20_21[["Funciones.Usadas"]]),";")
Signal20_21$N.Funciones.Usadas <- as.numeric(lapply(funciones,length))

funciones.usadas <- data.frame()
fila=0
# recorremos la lista de funciones mencionada por cada persona
for (por.persona in funciones)
{
  fila=fila+1
  # añadimos una columna por función 
  for(funcion in por.persona)
  {
    funciones.usadas[fila,funcion]<-1
  }
}

### Satisfacion.General.Signal como factor 
#Signal20_21$Factor.Satisfacion.General <- factor(Signal20_21$Satisfacion.General.Signal)





#################### Analisis #############################

# Mostrar tabla de resumen
table(Signal20_21$N.Funciones.Usadas,Signal20_21$Factor.Satisfacion.General)
ftable(xtabs(~Factor.Satisfacion.General+N.Funciones.Usadas, data = Signal20_21))

# regresión logística entre Satisfaccion y Numero de funciones
mod.sat.fun <- polr(factor(Satisfacion.General.Signal) ~ N.Funciones.Usadas, data = Signal20_21, Hess=TRUE,
                    method="probit")
summary(mod.sat.fun)

# Intervalos confianza
(ci <- confint(mod.sat.fun))
exp(cbind(OR = coef(mod.sat.fun), ci))

# generamos estimacion 
pred.sat <- data.frame(N.Funciones.Usadas=rep(1:20))
pred.sat <- cbind(pred.sat, predict(mod.sat.fun, pred.sat, type = "probs"))
lnewdat <-  melt(pred.sat, id.vars = c("N.Funciones.Usadas"),
                 variable.name = "Level", value.name="Probability")
ggplot(lnewdat, aes(x = N.Funciones.Usadas, y = Probability, colour = Level)) +
  labs(x="Numero de Funciones Usadas",
       title="Nivel de satisfacción de una persona conforme conoce más funciones de Signal")+
  scale_colour_discrete(name="Satisfaccion",labels=c("Nada","Poco","Satisfecho","Mucho"))+
  geom_line()



summary(update(mod.sat.fun,method="probit"))

Signal20_21$Aumenta.la.participacion

kk<-xtabs(~Genero+Satisfacion.General.Signal,data=Signal20_21)
summary(kk)

test.1<-Signal20_21 %>%
  dplyr::group_by(Satisfacion.General.Signal,Genero) %>%
  dplyr::summarise(Count=n()) 
indep.1<-glm(Count~ Satisfacion.General.Signal+Genero,data=indep.1,family=poisson())
summary(berk.ind)


######################## Gráficas #############################

# Resumen de género por contexto
p1 = ggplot(Signal20_21, aes(x = ContextoUso, fill = Genero)) + 
  theme_light() +
  labs(y="Numero de Usuarios",x="Contexto de Uso")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  geom_bar()
p1


datos2 <- Signal20_21 %>% gather(key = Benefits, value = Value, Aumenta.la.participacion:Ayuda.planificar.actividades)
datos2$Value<-factor(datos2$Value,
                         levels=c("Nada de acuerdo","Algo de acuerdo","Bastante de acuerdo","Muy de acuerdo"))

# Resumen de género por contexto
p2 = ggplot(datos2, aes(x=Benefits,fill=Value)) + 
  theme_light() +
  labs(y="Valoracion",x="Contexto de Uso",
       title="Valoracion Ventajas Uso Signal")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 4))+
  scale_fill_manual(values=c("#4D4D4D", "#385E9D", "#FFC845","#D22020"))+
  geom_bar()+
  coord_flip()
p2

datos3 <- Signal20_21 %>% gather(key = Benefits, value = Value, Comparacion.fácil.de.usar:Comparacion.trato.cercano..docente)
datos3$Value<-factor(datos3$Value,
                     levels=c("Nada de acuerdo","Algo de acuerdo","Bastante de acuerdo","Muy de acuerdo"))

# Resumen de género por contexto
p3 = ggplot(datos3, aes(x=Benefits,fill=Value)) + 
  theme_light() +
  labs(y="Valoracion",x="Contexto de Uso",
       title="Comparacion Signal con otras herramientas")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 4))+
  scale_fill_manual(values=c("#4D4D4D", "#385E9D", "#FFC845","#D22020"))+
  geom_bar()+
  coord_flip()
p3

names(Signal20_21)


datos4 <- Signal20_21 %>% gather(key = Benefits, value = Value, Informacion.adecuada.Instalacion:Informacion.adecuada.Financiacion)
datos4$Value<-factor(datos4$Value,
                     levels=c("Nada de acuerdo","Algo de acuerdo","Bastante de acuerdo","Muy de acuerdo"))

# Resumen de género por contexto
p4 = ggplot(datos4, aes(x=Benefits,fill=Value)) + 
  theme_light() +
  labs(y="Valoracion",x="Contexto de Uso",
       title="Satisfaccion con la información proporcionada para uso de Signal")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 4))+
  scale_fill_manual(values=c("#4D4D4D", "#385E9D", "#FFC845","#D22020"))+
  geom_bar()+
  coord_flip()
p4

# Comparativa funciones conocidas y opinion 
p5 = ggplot(Signal20_21,aes(x=N.Funciones.Usadas,
                            y=Satisfacion.General.Signal,
                            color=Satisfacion.General.Signal)) + 
  theme_light() +
  labs(y="Satisfacción General con Signal",x="Numero de Funciones Usadas",
       title="Comparamos Conocimiento Funcionalidades de Signal con la Valoracion de la Aplicacion")+
  scale_color_gradient(low="#FFC845",high="#D22020")+
  geom_count()+
  scale_size_area(max_size=12)
p5


# Resumen de género por contexto
funciones.usadas %>%   
  gather() %>% 
  filter(value == 1) %>% 
  group_by(key) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=key,y=n,fill=rep(c("claro","oscuro"),12))) + 
  theme_light() +
  labs(y="Numero Respuestas",x="Funciones usadas",
       title="Uso de las funciones de Signal")+
  geom_col()+
  scale_fill_manual(values=c( "#D22020", "#4D4D4D"))+
  theme(legend.position = "none")+
  coord_flip()


# Repetir o usar Signal en otras asignaturas
Signal20_21 %>%
  dplyr::select(Utilizar.Signal.En.Otras.Asignaturas) %>%
  group_by(Utilizar.Signal.En.Otras.Asignaturas) %>%
  count() %>%
  ungroup() %>%
  mutate(Respuesta = factor(Utilizar.Signal.En.Otras.Asignaturas,
                            levels=c("Sí","No")),
         cumulative = cumsum(n),
         midpoint = cumulative - n / 2,
         labels = paste0(round((n/ sum(n)) * 100, 1), "%")) %>%
ggplot(aes(x="",y=n,
           fill=Respuesta)) + 
  theme_light() +
  labs(y="",x="",fill="Respuesta:",
       title="¿Te gustaría utilizar Signal en otras asignaturas y/o actividades académicas?")+
  geom_bar(width = 1, stat = "identity")+
  scale_fill_manual(values=c( "#D22020","#797979" ))+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())+
  geom_text(aes(x=1.2, y = midpoint, 
                label = labels),color="black",fontface="bold")+
  coord_polar(theta="y", start=0) 
