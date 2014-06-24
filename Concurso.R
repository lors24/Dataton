## Directorio de traabjo
setwd("/Users/loredp/Documents/Dataton")

## Bibliotecas
library(foreign)
library(plyr)
install.packages("xlsx")
library(xlsx)

## Lectura de bases

hechos <- read.csv("./hechos.csv", header = T, as.is = T)
detenidos <- read.csv("./detenidos.csv", header = T, skip=2, as.is = T)

## Nombres en minusculas

names(hechos) <- tolower(names(hechos))
names(detenidos) <- names(hechos)

## Eliminar ultimos 5 renglones en ambas bases (vacios)

hechos <- hechos[1:4906, ]
detenidos <- detenidos[1:4936, ]







# Para limpiar cada base.

# Base vivienda
Vialidad <- read.dbf("jal_eje_vial.dbf")
salud <- read.dbf("jal_cpv2010_loc_urb_servicios_de_salud.dbf")
var <- read.dbf("DescripcionVariables.dbf")
V$ID <- paste(V$CONTROL, V$VIV_SEL, sep = "#")
ser <- read.dbf("jal_servicios_a.dbf")



del <- read.csv("./detenidos.csv", header = T, skip=2)
hechos <- read.csv("./hechos.csv", header = T)
names(del)<-names(hechos)

table(del$DELITO)


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
