#Evolucion de los incendios forestales en México, datos obtenidos al 2017

#descargar la base de datos e integrarla dentro de la carpeta de incendios 
#forestales
library(readxl)
library(tidyverse)
library(gganimate)
library(magrittr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(hrbrthemes)

setwd("C:/Users/Adrian Bautista/Desktop/Incendios")
bases <- dir()

ctrl <- 2010

for (i in bases) {
  nombre <- paste("datos",ctrl,sep = "_")
  base <- read_excel(i)
  assign(nombre,base,envir = globalenv())
  ctrl <- ctrl + 1
}

rm(ctrl)
rm(i)
rm(nombre)
rm(Serie_historica_anual_incendios_2010_)
rm(bases)
rm(base)


coordenadas10 <- select(datos_2010, matches("coor"))
coordenadas11 <- select(datos_2011, matches("coor"))
coordenadas12 <- select(datos_2012, matches("coor"))
coordenadas13 <- select(datos_2013, matches("coor"))
coordenadas14 <- select(datos_2014, matches("coor"))
coordenadas15 <- select(datos_2015, matches("coor"))
coordenadas16 <- datos_2016[,2:7]
coordenadas17 <- datos_2017[,3:8]

afectaciones10 <- select(datos_2010, matches("Ha"))
afectaciones11 <- select(datos_2011, matches("Ha"))
afectaciones12 <- select(datos_2012, matches("hectareas"))
afectaciones13 <- select(datos_2013, matches("tamaño"))
afectaciones14 <- select(datos_2014, matches("tamaño"))
afectaciones15 <- select(datos_2015, matches("tamaño"))
afectaciones16 <- select(datos_2016, matches("tamaño"))
afectaciones17 <- select(datos_2017, matches("tamaño"))

afectaciones10 %<>% as.data.frame()
afectaciones11 %<>% as.data.frame()
afectaciones12 %<>% as.data.frame()
afectaciones13 %<>% as.data.frame()
afectaciones14 %<>% as.data.frame()
afectaciones15 %<>% as.data.frame()
afectaciones16 %<>% as.data.frame()
afectaciones17 %<>% as.data.frame()


datos_2010 %<>% as.data.frame()
datos_2011 %<>% as.data.frame()
datos_2012 %<>% as.data.frame()
datos_2013 %<>% as.data.frame()
datos_2014 %<>% as.data.frame()
datos_2015 %<>% as.data.frame()
datos_2016 %<>% as.data.frame()
datos_2017 %<>% as.data.frame()

coordenadas10 %<>% as.data.frame()
coordenadas11 %<>% as.data.frame()
coordenadas12 %<>% as.data.frame()
coordenadas13 %<>% as.data.frame()
coordenadas14 %<>% as.data.frame()
coordenadas15 %<>% as.data.frame()
coordenadas16 %<>% as.data.frame()
coordenadas17 %<>% as.data.frame()

info2010 <- cbind(coordenadas10,afectaciones10)
info2011 <- cbind(coordenadas11,afectaciones11)
info2012 <- cbind(coordenadas12,afectaciones12)
info2013 <- cbind(coordenadas13,afectaciones13)
info2014 <- cbind(coordenadas14,afectaciones14)
info2015 <- cbind(coordenadas15,afectaciones15)
info2016 <- cbind(coordenadas16,afectaciones16)
info2017 <- cbind(coordenadas17,afectaciones17)

#procedimiento especial para cada año, no homologados
View(info2013)

info2010 <- info2010[,-c(2,3)]
info2011 <- info2011[,-c(2,3)]
info2015 <- info2015[,-1]

#2010
info2010 <- separate(info2010,Coordenadas,sep = "-",into = c("Latitud","Longitud"))
info2010 <- mutate(info2010,Tamaño = info2010$`Renuevo (Ha)`+info2010$`Arbolado Adulto (Ha)`+
                     info2010$`Matorral Arbustivo (Ha)`+info2010$`Pastizal (Ha)`)
info2010 <- info2010[,-c(3:6)]

class(info2010$Longitud)
head(info2010$Latitud)

info2010$Longitud <- sub(".", "", info2010$Longitud)
info2010$Longitud <- sub(".", "", info2010$Longitud)

info2010[5351,]

info2010 <- separate(info2010,Longitud,sep = " ",into = c("Grados_Lat","Minutos_Lat","Segundos_Lat"))

info2010$Latitud <- sub(".$", "", info2010$Latitud)
info2010$Latitud <- sub(".$", "", info2010$Latitud)

info2010 <- separate(info2010,Latitud,sep = " ",into = c("Grados_Lon","Minutos_Lon","Segundos_Lon"))

class(info2010$Tamaño)

info2010$Tamaño<-cut(info2010$Tamaño, c(-1,5,10,20,50,100,1000000000))

levels(info2010$Tamaño) <- c("0 a 5 Hectáreas","6 a 10 Hectáreas",
                             "11 a 20 Hectáreas","21 a 50 Hectáreas",
                             "51 a 100 Hectáreas","Mayor a 100 Hectáreas")


info2010$Año <- 2010
info2010 <- na.omit(info2010)

#2011
info2011 <- separate(info2011,Coordenadas,sep = "-",into = c("Latitud","Longitud"))
info2011 <- mutate(info2011,Tamaño = info2011$`Renuevo (Ha)`+info2011$`Arbolado Adulto (Ha)`+
                     info2011$`Matorral Arbustivo (Ha)`+info2011$`Pastizal (Ha)`)
info2011 <- info2011[,-c(3:6)]

info2011$Longitud <- sub(".", "", info2011$Longitud)
info2011$Longitud <- sub(".", "", info2011$Longitud)


info2011 <- separate(info2011,Longitud,sep = " ",into = c("Grados_Lat","Minutos_Lat","Segundos_Lat"))

info2011$Latitud <- sub(".$", "", info2011$Latitud)
info2011$Latitud <- sub(".$", "", info2011$Latitud)

info2011 <- separate(info2011,Latitud,sep = " ",into = c("Grados_Lon","Minutos_Lon","Segundos_Lon"))

info2011$Tamaño<-cut(info2011$Tamaño, c(-1,5,10,20,50,100,1000000000))

levels(info2011$Tamaño) <- c("0 a 5 Hectáreas","6 a 10 Hectáreas",
                             "11 a 20 Hectáreas","21 a 50 Hectáreas",
                             "51 a 100 Hectáreas","Mayor a 100 Hectáreas")

info2011$Año <- 2011
info2011 <- na.omit(info2011)

#2012
info2012 <- separate(info2012,Coordenadas,sep = "-",into = c("Latitud","Longitud"))

head(info2012$Latitud)
head(info2012$Longitud)

info2012 <- separate(info2012,Latitud,sep = "°",into = c("Grados_Lat","Separar"))
info2012 <- separate(info2012,Separar,sep = "´",into = c("Minutos_Lat","Segundos_Lat"))

info2012$Segundos_Lat <- sub(".$", "", info2012$Segundos_Lat)
info2012$Segundos_Lat <- sub(".$", "", info2012$Segundos_Lat)

info2012 <- separate(info2012,Longitud,sep = "°",into = c("Grados_Lon","Separar"))
info2012 <- separate(info2012,Separar,sep = "´",into = c("Minutos_Lon","Segundos_Lon"))

info2012$Segundos_Lon <- sub(".$", "", info2012$Segundos_Lon)

info2012$Tamaño<-cut(info2012$hectareas, c(-1,5,10,20,50,100,1000000000))
info2012$hectareas <- NULL

levels(info2012$Tamaño)

levels(info2012$Tamaño) <- c("0 a 5 Hectáreas","6 a 10 Hectáreas",
                             "11 a 20 Hectáreas","21 a 50 Hectáreas",
                             "51 a 100 Hectáreas","Mayor a 100 Hectáreas")


info2012 <- info2012[,c(4,5,6,1,2,3,7)]


info2012$Año <- 2012

info2012 <- na.omit(info2012)

#2013

View(info2013)

info2013 <- separate(info2013,Coordenadas,sep = "-",into = c("Latitud","Longitud"))

info2013 <- separate(info2013,Latitud,sep = "°",into = c("Grados_Lat","Separar"))
info2013 <- separate(info2013,Separar,sep = "´",into = c("Minutos_Lat","Segundos_Lat"))

info2013$Segundos_Lat <- sub(".$", "", info2013$Segundos_Lat)
info2013$Segundos_Lat <- sub(".$", "", info2013$Segundos_Lat)

info2013$Longitud <- sub(".", "", info2013$Longitud)

info2013 <- separate(info2013,Longitud,sep = "°",into = c("Grados_Lon","Separar"))
info2013 <- separate(info2013,Separar,sep = "´",into = c("Minutos_Lon","Segundos_Lon"))

info2013$Segundos_Lon <- sub(".$", "", info2013$Segundos_Lon)

info2013$Tamaño <- as.factor(info2013$Tamaño)

levels(info2013$Tamaño) <- c("0 a 5 Hectáreas","11 a 20 Hectáreas",
                             "21 a 50 Hectáreas","51 a 100 Hectáreas",
                             "6 a 10 Hectáreas","Mayor a 100 Hectáreas")

info2013$Tamaño <- relevel(info2013$Tamaño,"Mayor a 100 Hectáreas")
info2013$Tamaño <- relevel(info2013$Tamaño,"51 a 100 Hectáreas")
info2013$Tamaño <- relevel(info2013$Tamaño,"21 a 50 Hectáreas")
info2013$Tamaño <- relevel(info2013$Tamaño,"11 a 20 Hectáreas")
info2013$Tamaño <- relevel(info2013$Tamaño,"6 a 10 Hectáreas")
info2013$Tamaño <- relevel(info2013$Tamaño,"0 a 5 Hectáreas")

info2013 <- info2013[,c(4,5,6,1,2,3,7)]

info2013$Año <- 2013

info2013 <- na.omit(info2013)

#2014
info2014 <- separate(info2014,Coordenadas,sep = "-",into = c("Latitud","Longitud"))

info2014 <- separate(info2014,Latitud,sep = "°",into = c("Grados_Lat","Separar"))
info2014 <- separate(info2014,Separar,sep = "´",into = c("Minutos_Lat","Segundos_Lat"))

info2014$Segundos_Lat <- sub(".$", "", info2014$Segundos_Lat)
info2014$Segundos_Lat <- sub(".$", "", info2014$Segundos_Lat)

info2014$Longitud <- sub(".", "", info2014$Longitud)

info2014 <- separate(info2014,Longitud,sep = "°",into = c("Grados_Lon","Separar"))
info2014 <- separate(info2014,Separar,sep = "´",into = c("Minutos_Lon","Segundos_Lon"))

info2014$Segundos_Lon <- sub(".$", "", info2014$Segundos_Lon)

info2014$Tamaño <- as.factor(info2014$Tamaño)
levels(info2014$Tamaño)

levels(info2014$Tamaño) <- c("0 a 5 Hectáreas","11 a 20 Hectáreas",
                             "21 a 50 Hectáreas","51 a 100 Hectáreas",
                             "6 a 10 Hectáreas","Mayor a 100 Hectáreas")


info2014$Tamaño <- relevel(info2014$Tamaño,"Mayor a 100 Hectáreas")
info2014$Tamaño <- relevel(info2014$Tamaño,"51 a 100 Hectáreas")
info2014$Tamaño <- relevel(info2014$Tamaño,"21 a 50 Hectáreas")
info2014$Tamaño <- relevel(info2014$Tamaño,"11 a 20 Hectáreas")
info2014$Tamaño <- relevel(info2014$Tamaño,"6 a 10 Hectáreas")
info2014$Tamaño <- relevel(info2014$Tamaño,"0 a 5 Hectáreas")

info2014 <- info2014[,c(4,5,6,1,2,3,7)]

nombres <- names(info2014)

info2014$Año <- 2014

info2014 <- na.omit(info2014)

#2015

names(info2015) <- c(nombres)
class(info2015$Tamaño)
info2015$Tamaño <- as.factor(info2015$Tamaño)
levels(info2015$Tamaño)

levels(info2015$Tamaño) <- c("0 a 5 Hectáreas","0 a 5 Hectáreas","11 a 20 Hectáreas",
                             "21 a 50 Hectáreas","51 a 100 Hectáreas","6 a 10 Hectáreas","Mayor a 100 Hectáreas")


info2015$Tamaño <- relevel(info2015$Tamaño,"Mayor a 100 Hectáreas")
info2015$Tamaño <- relevel(info2015$Tamaño,"51 a 100 Hectáreas")
info2015$Tamaño <- relevel(info2015$Tamaño,"21 a 50 Hectáreas")
info2015$Tamaño <- relevel(info2015$Tamaño,"11 a 20 Hectáreas")
info2015$Tamaño <- relevel(info2015$Tamaño,"6 a 10 Hectáreas")
info2015$Tamaño <- relevel(info2015$Tamaño,"0 a 5 Hectáreas")

info2015 <- info2015[,c(4,5,6,1,2,3,7)]

info2015$Año <- 2015

info2015 <- na.omit(info2015)


#2016

names(info2016) <- c(nombres)
class(info2016$Tamaño)
info2016$Tamaño <- as.factor(info2016$Tamaño)
levels(info2016$Tamaño)

info2016$Tamaño <- relevel(info2016$Tamaño,"Mayor a 100 Hectáreas")
info2016$Tamaño <- relevel(info2016$Tamaño,"51 a 100 Hectáreas")
info2016$Tamaño <- relevel(info2016$Tamaño,"21 a 50 Hectáreas")
info2016$Tamaño <- relevel(info2016$Tamaño,"11 a 20 Hectáreas")
info2016$Tamaño <- relevel(info2016$Tamaño,"6 a 10 Hectáreas")
info2016$Tamaño <- relevel(info2016$Tamaño,"0 a 5 Hectáreas")

info2016 <- info2016[,c(4,5,6,1,2,3,7)]

info2016$Año <- 2016

info2016 <- na.omit(info2016)


#2017

class(info2017$Tamaño)
info2017$Tamaño %<>% as.factor()
levels(info2017$Tamaño)
names(info2017) <- c(nombres)


info2017$Tamaño <- relevel(info2017$Tamaño,"Mayor a 100 Hectáreas")
info2017$Tamaño <- relevel(info2017$Tamaño,"51 a 100 Hectáreas")
info2017$Tamaño <- relevel(info2017$Tamaño,"21 a 50 Hectáreas")
info2017$Tamaño <- relevel(info2017$Tamaño,"11 a 20 Hectáreas")
info2017$Tamaño <- relevel(info2017$Tamaño,"6 a 10 Hectáreas")
info2017$Tamaño <- relevel(info2017$Tamaño,"0 a 5 Hectáreas")

info2017 <- info2017[,c(4,5,6,1,2,3,7)]

info2017$Año <- 2017

info2017 <- na.omit(info2017)

#renombrar 2015, 2016, 2017

names(info2015) <- names(info2014)
names(info2016) <- names(info2014)
names(info2017) <- names(info2014)

#hacer un joint de los df generados

Incendios_2010_2017 <- rbind(info2010,info2011,info2012,info2013,info2014,info2015,
                             info2016,info2017) 

Incendios_2010_2017$Grados_Lon %<>% as.numeric()
Incendios_2010_2017$Minutos_Lon %<>% as.numeric()
Incendios_2010_2017$Segundos_Lon %<>% as.numeric()

Incendios_2010_2017$Grados_Lat %<>% as.numeric()
Incendios_2010_2017$Minutos_Lat %<>% as.numeric()
Incendios_2010_2017$Segundos_Lat %<>% as.numeric()

Incendios_2010_2017 <- na.omit(Incendios_2010_2017)


Incendios_2010_2017$Grados_Lon <- abs(Incendios_2010_2017$Grados_Lon)
Incendios_2010_2017$Grados_Lat <- abs(Incendios_2010_2017$Grados_Lat)

obtener <- Incendios_2010_2017$Grados_Lon >= 87 & Incendios_2010_2017$Grados_Lon <= 117
Incendios_2010_2017 <- Incendios_2010_2017[obtener,]

obtener <- Incendios_2010_2017$Grados_Lat >= 14.5 & Incendios_2010_2017$Grados_Lat <= 33
Incendios_2010_2017 <- Incendios_2010_2017[obtener,]


#Eliminar sistematicamente todos los NA
Incendios_2010_2017 <- na.omit(Incendios_2010_2017)

Incendios_2010_2017$Año %<>% as.factor()


str(Incendios_2010_2017)

Incendios_2010_2017$Latitud <- Incendios_2010_2017$Grados_Lat +
  (Incendios_2010_2017$Minutos_Lat/60) + (Incendios_2010_2017$Segundos_Lat/3600)

Incendios_2010_2017$Longitud <- -1 * Incendios_2010_2017$Grados_Lon +
  (Incendios_2010_2017$Minutos_Lon/60) + (Incendios_2010_2017$Segundos_Lon/3600)


#corregir la longitud (ajustar)
Incendios_2010_2017$Longitud <- Incendios_2010_2017$Longitud - 1

mexico_map <- map_data("world", region = "Mexico")

mapa <-  ggplot(mexico_map, aes(x = long, y = lat)) + 
            geom_polygon(aes( group = group), fill="grey") + theme_minimal() +
            geom_point(data = Incendios_2010_2017,aes(x=Longitud,y=Latitud,size=Tamaño),alpha=.2,color="red") +
            transition_states(Año, transition_length = 4, state_length = 1, wrap = FALSE) +
            enter_fade() + exit_fade() + scale_size_discrete() +
            labs(title = "Incendios forestales registrados en México",
            subtitle="Año {closest_state}", x = element_blank(),
            y = element_blank(), caption = "Source: CONAFOR / Elaboracion: @4drian.bautista") + 
            theme_ft_rc() + theme(plot.subtitle=element_text(size=32, color="grey"))

animate(mapa,fps = 25, duration = 20,width = 960, height = 540)

anim_save("Incendios2.gif")

#grafica 2

paleta_de_colores <- colorRampPalette(c("Green","Red"))


mapa2 <-  ggplot(mexico_map, aes(x = long, y = lat)) + 
  geom_polygon(aes( group = group), fill="grey") + theme_minimal() +
  geom_point(data = Incendios_2010_2017,aes(x=Longitud,y=Latitud,color=Tamaño),alpha=.5) +
  transition_states(Año, transition_length = 4, state_length = 1, wrap = FALSE) +
  enter_fade() + exit_fade() +
  labs(title = "Incendios forestales registrados en México",
       subtitle="Año {closest_state}", x = element_blank(),
       y = element_blank(), caption = "Source: CONAFOR / Elaboracion: @4drian.bautista") + 
  theme_ft_rc() + theme(plot.subtitle=element_text(size=32, color="grey"))

animate(mapa2,fps = 25, duration = 20,width = 960, height = 540)

anim_save("Incendios3.gif")
