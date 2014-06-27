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

hechos <- hechos[, -1]
colonias <- colonias[, c(3:11,13:16, 21:34, 37, 39, 40)]

## Analisis de colonias

hechos$colonia <- gsub("\303\221", "N",hechos$colonia)
colonias$NomColonia <- toupper(colonias$NomCol)
colonias$NomColonia <- gsub("\351", "E", colonias$NomColonia)
colonias$NomColonia <- gsub("\363", "O", colonias$NomColonia)
colonias$NomColonia <- gsub("\355", "I", colonias$NomColonia)
colonias$NomColonia <- gsub("\341|\301", "A", colonias$NomColonia)
colonias$NomColonia <- gsub("\361", "N", colonias$NomColonia)
colonias$NomColonia <- gsub(",", "", colonias$NomColonia)

col_h[col_h %in% colonias$NomCol]

all <- merge(hechos, colonias, by.x = "colonia", by.y = "NomColonia" )

#hechos$fecha2 <- dmy(hechos$fecha, tz = "America, Mexico_City")
#hechos$diaSem <- wday(hechos$fecha2, label = T)
#hechos$mes    <- month(hechos$fecha2)
#hechos$fuente <- gsub("JUZGADOS MUNICIPALES", "juzgados_mun", hechos$fuente)
#hechos$fuente <- gsub("CENTRO DE RESPUESTA INMEDIATA ZAPOPAN", "centro_resp", hechos$fuente)

## Ordenamos por fecha

## Melt

allMelt <- melt(all, id=c("fecha2", "diaSem", "colonia"), measure.vars = c("delito"))
allF <- dcast(allMelt, fecha2 ~ variable, length)  #por fecha
allD <- dcast(allMelt, diaSem ~ variable, length) #por dia de la semana
allC <- dcast(allMelt, colonia ~ variable, length) #por colonia

allMelt2 <- melt(subAll, id = "delito", measure.vars = "diaSem")
allDel <- dcast(allMelt2, delito ~variable, length)

barplot(allD$delito, names.arg = c("L"),
        xlab = "Dia de la semana", ylab = "Frecuencia",
        col= 'blue', axisnames = T)
barplot(allF$delito, names.arg = allF$fecha2,
        xlab = "Dia", ylab = "Frecuencia", 
        col = rainbow(7), axes = T, axisnames = T)
plot(x=1:90, y=allF$delito, type = "b", xlab= "Dias", col = 'blue')
lines(spline(1:90,allF$delito))


barplot(allD$delito[c(2,6,7,5,1,3,4)], names.arg= allD$diaSem,
        legend.text = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"),
        col = rainbow(7), xlab = "Dia de la semana",
        ylab = "Frecuencia", axisnames = T, axes = T,
        xpd = F, inside = T)

## Limpiar tipos de delito

delitos <- sort(unique(all$delito, rowname = T))
## Hay 120 tipos de delitos. Vamos a elegir los relevantes.

## Eliminamos los delitos que ocurrieron una o dos veces unicamente
delitos <- delitos[!delitos %in% (allDel[allDel$fecha2<=2,1])]
delitos <- delitos[c(2:5, 31, 36:41, 47:54, 59:61, 66:83, 91:95, 103, 104)]  
subAll <- subset(all, all$delito %in% delitos)

## Eventos
names(eventos) <- tolower(names(eventos))

eventos$inicio <- as.POSIXlt(dmy(eventos$autorizacion, tz = "America, Mexico_City"))
eventos$fin <- as.POSIXlt(dmy(eventos$vencimiento, tz = "America, Mexico_City"))
eventos$duracion <-(eventos$fin-eventos$inicio)/(3600*24) # en dias

## Partidos

fechas_partidos <- eventos[grep("PARTIDO", eventos$observaciones),"inicio"]
fechas_conciertos <- eventos[grep("CONCIERTO", eventos$observaciones),]
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
MyMap <- GetMap(center=center, zoom=12 ,markers=markers,destfile = "./Zapopan.png")


## Se prob?? darle el cruce pero salian resultados con errores

#dir <- paste(hechos$calle, hechos$colonia, "Zapopan", sep = ",")

## Se hace en 2 partes porque el API de Google permiete un max de 2500 en 24 horas

#DF = cbind.data.frame(address = dir[1:2500], lat = NA, lon = NA)
#DF <-with(DF, data.frame(address, t(sapply(DF$address,getGeoCode))))
#DF_f <- DF[!complete.cases(DF),]
#DF[!complete.cases(DF),] <- with(DF_f, data.frame(address, t(sapply(DF_f$address, getGeoCode))))
#DF2 = cbind.data.frame(address = dir[2501:4906], lat = NA, lon = NA)
#DF2 <-with(DF2, data.frame(address, t(sapply(DF2$address,getGeoCode))))
#DF_f2 <- DF2[!complete.cases(DF2),]
#DF2[!complete.cases(DF2),] <- with(DF_f2, data.frame(address, t(sapply(DF_f2$address, getGeoCode))))

#Coord2 <- rbind(DF,DF2)
hechos$dir <- Coord2$address
hechos$lat <-all$lat
hechos$lon <- Coord2$lon
hechos <- hechos[,c("fecha2", "calle", "cruce", "colonia", "delito", "diaSem", "mes", "lat", "lon")]


Coord2[Coord2$lat >=20.85,]
longi<-Coord2[Coord2$lat >=20.85,3]
## Quitar los puntos que no son de Zapopan

lat <- subAll$lat
lon <- subAll$lon

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
    S <- subset(subAll, subAll$fecha2 == d)
    lt <- S$lat
    ln <- S$lon
    PlotOnStaticMap(MyMap, lat= lt, lon = ln, destfile = "./Zapopan_p8.png",
                   FUN= points, zoom = NONE , 
                   col=c('red'))
    PlotOnStaticMap(MyMap, lat= OM[1], lon = OM[2], destfile = "./Zapopan_p8.png",
                    FUN= points, zoom = NONE , add = T,
                    col=c('blue'))
    TextOnStaticMap(MyMap, lat = 20.76187, lon = -103.5074, labels = as.character(d), TrueProj = TRUE, 
                    FUN = text, add = TRUE, verbose = 0)
    TextOnStaticMap(MyMap, lat = 20.68731, lon = -103.477, labels = "Estadio Omnilife", TrueProj = TRUE, 
                    FUN = text, add = TRUE, verbose = 0)
    return(dim(S)[1])
}

dp <- as.character(fechas_partidos)
sapply(dp,graf)

par <- allF[!allF$fecha2 %in% as.character(eventos$inicio),]
summary(par)
summary(allF$delito)

## Graficar por tipo de delito

IdentifyPoints(MyMap, n = 1, verbose = 0)

grafd <- function (d){
    S <- subset(all, all$delito == d)
    lt <- S$lat
    ln <- S$lon
    PlotOnStaticMap(MyMap, lat= lt, lon = ln, destfile = "./Zapopan_p8.png",
                              FUN= points, zoom = NONE , 
                              col=c('red'))
    TextOnStaticMap(MyMap, lat = 20.73687, lon = -103.4789, labels = as.character(d), TrueProj = TRUE, 
                    FUN = text, add = TRUE, verbose = 0)
}

grafd("ROBO VEHICULO")

lapply("ROBO VEHICULO", grafd)

## Graficar por colonia

grafc <- function (d){
    S <- subset(all, all$colonia == d)
    lt <- S$lat
    ln <- S$lon
    lat = mean(lt)
    lon = mean(ln)
    center = c(lat, lon)
    markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&markers=color:",
                     "green|label:G|40.711614,-74.012318&markers=color:red|color:red|",    
                     "label:C|40.718217,-73.998284")
    MyMap2 <- GetMap(center=center, zoom=16 ,markers=markers,destfile = "./Zapopan.png")
    
    PlotOnStaticMap(MyMap2, lat= lt, lon = ln, destfile = "./Zapopan_p8.png",
                              FUN = points, zoom = NONE , 
                              col=c('purple'))
    TextOnStaticMap(MyMap2, lat = lat+0.005, lon = lon, labels = d, TrueProj = TRUE, 
                    FUN = text, add = TRUE, verbose = 0)
    return(dim(S)[1])
}

sapply(sort(unique(all$colonia)), grafc)

## Escribir datos

write.csv(Coord,"./coord.csv")
write.csv(Coord2, "./coord2.csv")
write.csv(hechos, "./hechos_f.csv")

## Graficar por tipos de delitos

PlotOnStaticMap(MyMap, lat, lon, destfile = "./Zapopan_p8.png",
                FUN= points, zoom = 2 , 
                col=c('blue'))

## Auditorio Telmex

AT <- getGeoCode("Auditorio Telmex")
OM <- getGeoCode("Estadio Omnilife")
VU <- getGeoCode("Villa Univeristaria, Zapopan")

## Colonias conflicitvas

ColC <- allC[allC$delito>30,]

sapply(ColC$colonia, grafc)

colC <- merge(ColC, colonias, by.x = "colonia", by.y = "NomColonia" )
colC <- colC[, c("colonia", "delito", "CalleAlumbrado.",  "ModuloPoli")]
