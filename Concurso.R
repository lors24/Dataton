## Directorio de traabjo
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

## Lectura de bases

hechos <- read.csv("./hechos.csv", header = T, as.is = T)
detenidos <- read.csv("./detenidos.csv", header = T, skip=2, as.is = T)
eventos <- read.csv("./eventos.csv", as.is = T)

## Nombres de variables en minusculas

names(hechos) <- tolower(names(hechos))
names(detenidos) <- names(hechos)
names(eventos) <-tolower(names(eventos))
names(eventos) <- gsub("\\.", "", names(eventos))

## Quitar renglones vacios

hechos <- hechos[1:4906, ]
detenidos <- detenidos[1:4936, ]
eventos <- eventos[1:106,]

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

## Ordenamos por fecha

hechos <- hechos[order(hechos$fecha2), ]

## Melt

hechosMelt <- melt(hechos, id=c("fecha2", "diaSem"), measure.vars = c("delito"))
hechosF <- dcast(hechosMelt, fecha2 ~ variable, length) 
hechosD <- dcast(hechosMelt, diaSem ~ variable, length)
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


delitos <- delitos[c(2,)]
delitos <- delitos[c(1:5, 8:11, 14:22, 24, 26, 27, 30:31, 34, 36:39, 43:51, 54, 57:58, 63:64, 67:69, 70:71, 75:78, 81:83, 85:87, 89:91, 93:94, 98:102, 108, 111:112, 114, 116:120 )]




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

grep("PARTIDO", eventos$observaciones)
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
lat = 20.6837308
lon = -103.4263421
center = c(lat, lon)
markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&markers=color:",
                 
                 
                 "green|label:G|40.711614,-74.012318&markers=color:red|color:red|",
                 
                 
                 "label:C|40.718217,-73.998284")
MyMap <- GetMap(center=center, zoom=13 ,markers=markers,destfile = "Zapopan.png")
MyMap

## Graficar puntos
PlotOnStaticMap(MyMap, lat, lon, destfile = "./Zapopan1.png",
                FUN= points, zoom = NONE , 
                col=c('red','blue','green'))
               
## Coordenadas
DF = cbind.data.frame(address=c("Zaruma 20, Lindavista"))
DF <- with(DF, data.frame(address, t(sapply(DF$address, getGeoCode))))
DF
lat <- DF$lat
lon <- DF$lon

hechos$calle

calles <- paste(hechos$calle, "y", hechos$cruce, sep = " ")
dir <- paste(calles, hechos$colonia, "Zapopan", sep = ",")
DF = cbind.data.frame(address = dir[1:10], lat = NA, lon = NA)
DF <-with(DF, data.frame(address, t(sapply(DF$address,getGeoCode))))
DF

