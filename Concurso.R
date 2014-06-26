## Directorio de trabajo
## Colocar las bases necesarias en el direcotrio de trabajo

setwd("/Users/loredp/Documents/Dataton")

## Bibliotecas
library(foreign)
library(plyr)

install.packages("xlsx")
library(xlsx)

install.packages("lubridate")
library(lubridate)
library(reshape2)

install.packages("ggplot2")
library(ggplot2)

install.packages("RgoogleMaps")
library(RgoogleMaps)

install.packages("animation")
library(animation)

## Lectura de bases

hechos <- read.csv("./hechos.csv", header = T, as.is = T)
detenidos <- read.csv("./detenidos.csv", header = T, skip=2, as.is = T)
eventos <- read.csv("./eventos.csv", as.is = T)
colonias <- read.csv("./Colonias.csv", as.is = T)

## Leer de la base construida

hechos <- read.csv("./hechos_f.csv", header = T, as.is = T, row.names = NULL)

## Nombres de variables en minusculas

names(hechos) <- tolower(names(hechos))
names(detenidos) <- names(hechos)
names(eventos) <-tolower(names(eventos))
names(eventos) <- gsub("\\.", "", names(eventos))

## Quitar renglones vacios

hechos <- hechos[1:4906, ]
detenidos <- detenidos[1:4936, ]
eventos <- eventos[1:106,]
colonias <- colonias[1:526, ]

## Elegir columnas

colonias <- colonias[, c(3:11,13:16, 21:34, 37, 39, 40)]

## Analisis exploratorio de datos de delitos

round(table(hechos$fuente)/nrow(hechos)*100,2)
table(detenidos$fuente)

## El 57.01% de los hechos se reportaron desde el centro de respuesta inmediata zapopan
## mientras que el 42.99% fue desde los juzgados municipales. En el caso de los detenidos
## la fuente es los juzgados municipales en todos los casos.

hechos$fecha2 <- dmy(hechos$fecha, tz = "America, Mexico_City")
hechos$diaSem <- wday(hechos$fecha2, label = T)
hechos$mes    <- month(hechos$fecha2)
hechos$fuente <- gsub("JUZGADOS MUNICIPALES", "juzgados_mun", hechos$fuente)
hechos$fuente <- gsub("CENTRO DE RESPUESTA INMEDIATA ZAPOPAN", "centro_resp", hechos$fuente)
h

## Ordenamos por fecha

hechos <- hechos[order(hechos$fecha2), ]

## Melt

hechosMelt <- melt(hechos, id=c("fecha2", "diaSem", "colonia"), measure.vars = c("delito"))
hechosF <- dcast(hechosMelt, fecha2 ~ variable, length) 
hechosD <- dcast(hechosMelt, diaSem ~ variable, length)
hechosC <- dcast(hechosMelt, colonia ~ variable, length)

# Cuantos hechos hubo por fecha y dia de la semana. Podria hacerse con table pero 
# tiene mejor presentacion asi.

hechosMelt2 <- melt(hechos, id = "delito", measure.vars = "fecha2")
hechosDel <- dcast(hechosMelt2, delito ~variable, length)

barplot(hechosD$delito, names.arg = hechosD$diaSem,
        xlab = "Dia de la semana", ylab = "Frecuencia", 
        col = rainbow(7), axes = T, axisnames = T)
barplot(hechosF$delito, names.arg = hechosF$fecha2,
        xlab = "Dia", ylab = "Frecuencia", 
        col = rainbow(7), axes = T, axisnames = T)
## Limpiar tipos de delito

table(hechos$delito)
delitos <- sort(unique(hechos$delito, rowname = T))
## Hay 120 tipos de delitos. Vamos a elegir los relevantes.

delitos <- delitos[!delitos %in% (hechosDel[hechosDel$fecha2<=2,1])]



delitos <- delitos[c(1:4, 7, 10, 15:18, 23:31, 35:42, 44, 46, 52:64, 68:71, 73, 74, 79, 80)]
subHechos <- subset(hechos, hechos$delito %in% delitos)
subHechos$delito[grep("ROBO", subHechos$delito)] <- "robo"
subHechos$delito[grep("OCCISO", subHechos$delito)] <- "occiso" 
table(subHechos$delito)

## Eventos
names(eventos) <- tolower(names(eventos))

eventos$inicio <- as.POSIXlt(dmy(eventos$autorizacion, tz = "America, Mexico_City"))
eventos$fin <- as.POSIXlt(dmy(eventos$vencimiento, tz = "America, Mexico_City"))
eventos$duracion <-(eventos$fin-eventos$inicio)/(3600*24) # en dias

## Partidos

grep("PARTIDO", eventos$observaciones, value = T)
grep("CONCIERTO", eventos$observaciones)
grep("OMNI", eventos$interior)
grep("TELMEX", eventos$interior)

gsub("\303\221", "N", del$DELITO)
gsub("J","\\.", del$DELITO)

unique(detenidos$DELITO)

(table((del$Fecha)))
names(del)

### Mapas

## Mapa Zapopan
lat = 20.67196
lon = -103.41650
lat = 20.6837308
lon = -103.4263421
center = c(lat, lon)
markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&markers=color:",
                 "green|label:G|40.711614,-74.012318&markers=color:red|color:red|",    
                 "label:C|40.718217,-73.998284")
MyMap <- GetMap(center=center, zoom=12 ,markers=markers,destfile = "./Zapopan.png")
MyMap

## Se prob?? darle el cruce pero salian resultados con errores

dir <- paste(hechos$calle, hechos$colonia, "Zapopan", sep = ",")

## Se hace en 2 partes porque el API de Google permiete un max de 2500 en 24 horas

DF = cbind.data.frame(address = dir[1:2500], lat = NA, lon = NA)
DF <-with(DF, data.frame(address, t(sapply(DF$address,getGeoCode))))
DF_f <- DF[!complete.cases(DF),]
DF[!complete.cases(DF),] <- with(DF_f, data.frame(address, t(sapply(DF_f$address, getGeoCode))))


DF2 = cbind.data.frame(address = dir[2501:4906], lat = NA, lon = NA)
DF2 <-with(DF2, data.frame(address, t(sapply(DF2$address,getGeoCode))))
DF_f2 <- DF2[!complete.cases(DF2),]
DF2[!complete.cases(DF2),] <- with(DF_f2, data.frame(address, t(sapply(DF_f2$address, getGeoCode))))


Coord2[grep("CABECERA MUNICIPAL", Coord2$address),]

Coord2 <- rbind(DF,DF2)
hechos$dir <- Coord2$address
hechos$lat <-Coord2$lat
hechos$lon <- Coord2$lon
hechos <- hechos[,c("fecha2", "calle", "cruce", "colonia", "delito", "diaSem", "mes", "lat", "lon")]


Coord2[Coord2$lat >=20.85,]
longi<-Coord2[Coord2$lat >=20.85,3]
## Quitar los puntos que no son de Zapopan

lat <- Coord2$lat
lon <- Coord2$lon

boxplot(lat,lon)

## Graficar puntos
PlotOnStaticMap(MyMap, lat, lon, destfile = "./Zapopan_p8.png",
                FUN= points, zoom = 2 , 
                col=c('blue'))

PlotOnStaticMap(MyMap, lat= lati, lon = longi, destfile = "./Zapopan_p8.png",
                FUN= points, zoom = NONE , 
                col=c('red'))

grep("Pinar de la Venta",colonias$NomCol)
colonias[269,]

summary(DF$lat)

## Graficar por dia

dia <- dmy("02/01/14", tz = "America, Mexico_City")
graf <- function (d){
    S <- subset(hechos, hechos$fecha2 == d)
    lt <- S$lat
    ln <- S$lon
    MyMap2 <- PlotOnStaticMap(MyMap, lat= lt, lon = ln, destfile = "./Zapopan_p8.png",
                   FUN= points, zoom = NONE , 
                   col=c('red'))
    TextOnStaticMap(MyMap2, lat = 20.75467, lon = -103.506, labels = as.character(d), TrueProj = TRUE, 
                    FUN = text, add = TRUE, verbose = 0)
}

dp <- unique(hechos$fecha2)[1:10]
lapply(dp,graf)

## Graficar por tipo de delito

grafd <- function (d){
    S <- subset(hechos, hechos$delito == d)
    lt <- S$lat
    ln <- S$lon
    MyMap2 <- PlotOnStaticMap(MyMap, lat= lt, lon = ln, destfile = "./Zapopan_p8.png",
                              FUN= points, zoom = NONE , 
                              col=c('red'))
    TextOnStaticMap(MyMap2, lat = 20.75467, lon = -103.506, labels = as.character(d), TrueProj = TRUE, 
                    FUN = text, add = TRUE, verbose = 0)
}

lapply(delitos[1:10], grafd)

## Escribir datos

write.csv(Coord,"./coord.csv")
write.csv(Coord2, "./coord2.csv")
write.csv(hechos, "./hechos_f.csv")