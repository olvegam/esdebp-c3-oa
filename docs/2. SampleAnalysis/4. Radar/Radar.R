# Set path to Desktop

# Para MAC
# setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/4. Radar")

# Para Windows Bogotá - portáti Unillanos. 
setwd("D:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/4. Radar")

# Para Windows Unillanos
#setwd("C:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/4. Radar")
# Instalaci??n de paquetes y librer??as

#Para leer archivos de Excel
#install.packages("tidyverse")
# Leer el paquete para lectura de archivos de Excel. 
library("readxl")

#Para utilizar ggplot y graficar los boxplot con t?tulos y dem?s. 
#install.packages("ggplot2")
library(ggplot2)

##############################################################
###   Leer los datos (cada caso y los de ambos casos)      ###
##############################################################

# Lea los datos para el caso de estudio FMSG.
datosFMSGForR <- read_excel("ScoreAverages.xlsx", sheet = 1)
# Lea los datos para el caso de estudio PdV.
datosPdVForR <- read_excel("ScoreAverages.xlsx", sheet = 2)
# Lea los datos para de ambos casos de estudio.
datosTwoCSForR <- read_excel("ScoreAverages.xlsx", sheet = 3)

###############################################################
###   Mostrar una porcion aleatoria de cada conjunto de datos.# 
###############################################################

# Show first a random sample of each data set. 
set.seed(1234)

#datosFMSGForR

# Summary of the data 
dplyr::sample_n(datosFMSGForR, 10)
summary(datosFMSGForR)

# Summary of the data 
dplyr::sample_n(datosPdVForR, 10)
summary(datosPdVForR)

# Summary of the data 
dplyr::sample_n(datosTwoCSForR, 10)
summary(datosTwoCSForR)

###############################################################
###   Cree los .png con los gráficos de Radar de cada       ###
###   conjunto de datos.                                    ### 
###############################################################

### Solicitud de Crédito FMSG

#Cree la gráfica de radar. 
rMSG = ggplot(datosFMSGForR, aes(x = Question, y = Score, col = Artifacts, group = Artifacts)) +
  labs(x = "Questions", y= "Score") + 
  ylim(c(1,5)) + geom_point() + geom_line() + geom_polygon(fill = NA) + 
  coord_polar(theta="x", start = 0, direction = 1) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(legend.position="bottom", legend.text = element_text(size = rel (1.5)), 
        legend.title = element_text(face="bold", size = rel(1.5)))

#Exportar la imagen a tiff - prepare el archivo. 
tiff(filename = "rq1-m1-msg-radar.tif", width = 2400, height = 2400, units = "px", 
     compression = "lzw", res = 400)
print(rMSG)
dev.off()

#Exportar la imagen a png - prepare, imprima, cierre el archivo. 
png(filename = "rq1-m1-msg-radar.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(rMSG)
dev.off()

#Exportar la imagen a pdf - prepare, imprima, cierre el archivo
pdf("rq1-m1-msg-radar.pdf")
print(rMSG)
dev.off()

### Punto de Venta
#Cree la gráfica de radar. 
rPdV = ggplot(datosPdVForR, aes(x = Question, y = Score, col = Artifacts, group = Artifacts)) +
  ylim(c(1,5)) + geom_point() + geom_line() + geom_polygon(fill = NA) + 
  coord_polar(theta="x", start = 0, direction = 1) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(legend.position="bottom", legend.text = element_text(size = rel (1.5)), 
        legend.title = element_text(face="bold", size = rel(1.5)))

#Exportar la imagen a tif - prepare el archivo. 
tiff(filename = "rq1-m1-ps-radar.tif", width = 2400, height = 2400, units = "px", 
     compression = "lzw", res = 400)
print(rPdV)
dev.off()

#Exportar la imagen a png - prepare, imprima, cierre el archivo. 
png(filename = "rq1-m1-ps-radar.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(rPdV)
dev.off()

#Exportar la imagen a pdf - prepare, imprima, cierre el archivo
pdf("rq1-m1-ps-radar.pdf")
print(rPdV)
dev.off()

### Radar ambos casos de estudio

#Cree la gráfica de radar. 
rBoth = ggplot(datosTwoCSForR, aes(x = Question, y = Score, col = Artifacts, group = Artifacts)) +
  ylim(c(1,5)) + geom_point() + geom_line() + geom_polygon(fill = NA) + 
  coord_polar(theta="x", start = 0, direction = 1) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(legend.position="bottom", legend.text = element_text(size = rel (1.5)), 
        legend.title = element_text(face="bold", size = rel(1.5)))

#Exportar la imagen a png - prepare el archivo. 
tiff(filename = "rq1-m1-Both-radar.tif", width = 2400, height = 2400, units = "px", 
     compression = "lzw", res = 400)
print(rBoth)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m1-Both-radar.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(rBoth)
dev.off()

#Exportar la imagen a pdf - prepare, imprima, cierre el archivo
pdf("rq1-m1-Both-radar.pdf")
print(rBoth)
dev.off()
