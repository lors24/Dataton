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

## Lectura de bases

hechos <- read.csv("./hechos.csv", header = T, as.is = T)
detenidos <- read.csv("./detenidos.csv", header = T, skip=2, as.is = T)

## Nombres de variables en minusculas

names(hechos) <- tolower(names(hechos))
names(detenidos) <- names(hechos)

## Eliminar ultimos 5 renglones en ambas bases (vacios)

hechos <- hechos[1:4906, ]
detenidos <- detenidos[1:4936, ]

## Analisis exploratorio de datos

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

barplot(hechosD$delito, names.arg = hechosD$diaSem,
        xlab = "Dia de la semana", ylab = "Frecuencia", 
        col = rainbow(7), axes = T, axisnames = T)
barplot(hechosF$delito, names.arg = hechosF$fecha2,
        xlab = "Dia", ylab = "Frecuencia", 
        col = rainbow(7), axes = T, axisnames = T)
## Limpiar tipos de delito

table(hechos$delito)
delitos <- unique(hechos$delito, rowname = T)
## Hay 120 tipos de delitos. Vamos a elegir los relevantes.

delitos <- delitos[c(1:5, 8:11, 14:22, 24, 26, 27, 30:31, 34, 36:39, 43:51, 54, 57:58, 63:64, 67:69, 70:71, 75:78, 81:83, 85:87, 89:91, 93:94, 98:102, 108, 111:112, 114, 116:120 )]
subHechos <- subset(hechos, hechos$delito %in% delitos)
subHechos$delito[grep("ROBO", subHechos$delito)] <- "robo"
subHechos$delito[grep("OCCISO", subHechos$delito)] <- "occiso" 
table(subHechos$delito)

# Base vivienda
Vialidad <- read.dbf("jal_eje_vial.dbf")
salud <- read.dbf("jal_cpv2010_loc_urb_servicios_de_salud.dbf")
var <- read.dbf("DescripcionVariables.dbf")
V$ID <- paste(V$CONTROL, V$VIV_SEL, sep = "#")
ser <- read.dbf("jal_servicios_a.dbf")

## Eventos

eventos <- read.csv("./eventos.csv", as.is = T)
names(eventos) <- tolower(names(eventos))
eventos <- eventos[1:106,]
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

colonias <- read.csv("./Colonias.csv")
eventos <- read.csv("./eventos.csv")
twitter <- read.csv("./Twitter.csv")
grep("secuestro", twitter$X_source_text, value = T)
grep("robo", twitter$X_source_text, value = T)
grep("alcohol", twitter$X_source_text, value = T)
