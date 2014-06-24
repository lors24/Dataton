getwd()
setwd("/Users/loredp/Downloads")

fileUrl<-"https://www.dropbox.com/sh/skv5nl9t5mdxr4n/AAD0Hz4XjcGRkhYdUebKjGCOa/Foursquare.csv"
download.file(fileUrl,destfile="/Users/loredp/Documents/foursquare.csv",method="curl")
fs<-read.csv("/Users/loredp/Downloads/Foursquare.csv")
head(fs)

festividades<-read.csv("/Users/loredp/Downloads/Festividades.csv")
head(festividades)

CentrosSalud<-read.csv("./CentrosSalud.csv")
head(CentrosSalud)

tel <- read.csv("./marzo_Zapopan201401.csv")
head(tel,25)

bp <- read.csv("./BibliotecasPublicas.csv")
head(bp)
