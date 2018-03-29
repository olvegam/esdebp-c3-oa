# Set path to Desktop

# Para MAC
#setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/5. Time")

# Para Windows
setwd("D:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/5. Time")

# Instalacion de paquetes y librerias

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

# Lea los datos para comparación de distribuciones. 
datosBoth <- read_excel("CR-TimeSc4y5.xlsx", sheet = 1)
datosMSG <- read_excel("CR-TimeSc4y5.xlsx", sheet = 5)
datosPS <- read_excel("CR-TimeSc4y5.xlsx", sheet = 9)

# Lea los datos de cada distribución por grupo de artefactos para SW test. 

datosBothB <- read_excel("CR-TimeSc4y5.xlsx", sheet = 2)
datosBothBU <- read_excel("CR-TimeSc4y5.xlsx", sheet = 3)
datosBothUC <- read_excel("CR-TimeSc4y5.xlsx", sheet = 4)

datosMSGB <- read_excel("CR-TimeSc4y5.xlsx", sheet = 6)
datosMSGBU <- read_excel("CR-TimeSc4y5.xlsx", sheet = 7)
datosMSGUC <- read_excel("CR-TimeSc4y5.xlsx", sheet = 8)

datosPSB <- read_excel("CR-TimeSc4y5.xlsx", sheet = 10)
datosPSBU <- read_excel("CR-TimeSc4y5.xlsx", sheet = 11)
datosPSUC <- read_excel("CR-TimeSc4y5.xlsx", sheet = 12)

###############################################################
###   Mostrar una porcion aleatoria de cada conjunto de datos.# 
###############################################################

# Show first a random sample of each data set. 
set.seed(1234)

# Summary of the data 
dplyr::sample_n(datosMSG, 10)
summary(datosMSG)

# Summary of the data 
dplyr::sample_n(datosPS, 10)
summary(datosPS)

# Summary of the data 
dplyr::sample_n(datosBoth, 10)
summary(datosBoth)

###############################################################
###   Organice los niveles de los datos.                    ### 
###############################################################

datosMSG$ArtifactsSet <- ordered(datosMSG$ArtifactsSet, levels(factor(datosMSG$ArtifactsSet)))
levels(datosMSG$ArtifactsSet)

datosPS$ArtifactsSet <- ordered(datosPS$ArtifactsSet, levels(factor(datosPS$ArtifactsSet)))
levels(datosPS$ArtifactsSet)

datosBoth$ArtifactsSet <- ordered(datosBoth$ArtifactsSet, levels(factor(datosBoth$ArtifactsSet)))
levels(datosBoth$ArtifactsSet)

###############################################################
###   Muestre los boxplot de cada conjunto de datos.        ### 
###############################################################

#Dibuje el boxplot con datos de FMSG y PS (Diagrama de cajas)
bMSG = ggplot(datosMSG, aes(ArtifactsSet,Time)) + geom_boxplot(outlier.shape = 19, width = 0.3) +
#  ggtitle ("Time Response of correct answers -  by case study (P=PS, F=FMSG)") +
  labs(x = "Artifacts set", y= "Tiempo de Respuesta") + 
  theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.x = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
#  theme(panel.grid.minor.y = element_line(colour = "grey50"))
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq2-m3-msg.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bMSG)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq2-m3-msg.png", width = 1800, height = 1200, units = "px", 
     res = 200)
print(bMSG)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq2-m3-msg.pdf")
print(bMSG)
dev.off()

#Dibuje el boxplot de PS. (Diagrama de cajas)
bPS = ggplot(datosPS, aes(ArtifactsSet,Time)) + geom_boxplot() +
#  ggtitle ("Case study point of sale") + 
  labs(x = "Artifacts set", y= "Time") + 
  theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.x = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.minor.y = element_line(colour = "grey50"))
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq2-m3-msg.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bPS)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq2-m3-msg.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(bPS)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq2-m3-msg.pdf")
print(bPS)
dev.off()

#Dibuje el boxplot de ambos casos de estudio. (Diagrama de cajas)
bBoth = ggplot(datosBoth, aes(ArtifactsSet,Time)) + geom_boxplot()+
#  ggtitle ("Time Response of correct answers - All case studies") +
  labs(x = "Artifacts set", y= "Tiempo de Respuesta") + 
  theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.x = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.minor.y = element_line(colour = "grey50"))
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq2-m3-both.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bBoth)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq2-m3-both.png", width = 1800, height = 1200, units = "px", 
     res = 200)
print(bBoth)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq2-m3-both.pdf")
print(bBoth)
dev.off()

###############################################################
###   Datos de cada distribución - Summary                  ### 
###############################################################

## Summary of the data - MSG 
FileSummaryMSGB = summary(datosMSGB)
FileSummaryMSGBU = summary(datosMSGBU)
FileSummaryMSGUC = summary(datosMSGUC)

## Summary of the data - PdV
FileSummaryPSB = summary(datosPSB)
FileSummaryPSBU = summary(datosPSBU)
FileSummaryPSUC= summary(datosPSUC)

# Summary of the data - BothCS 
FileSummaryBothB = summary(datosBothB)
FileSummaryBothBU = summary(datosBothBU)
FileSummaryBothUC = summary(datosBothUC)

###############################################################
###   Test de Shapiro Wilk                                  ### 
###############################################################

SwMSGB<- shapiro.test(datosMSGB$Time)
SwMSGBU<- shapiro.test(datosMSGBU$Time)
SwMSGUC<- shapiro.test(datosMSGUC$Time)
SwPSB<- shapiro.test(datosPSB$Time)
SwPSBU<- shapiro.test(datosPSBU$Time)
SwPSUC<- shapiro.test(datosPSUC$Time)
SwBothB<- shapiro.test(datosBothB$Time)
SwBothBU<- shapiro.test(datosBothBU$Time)
SwBothUC<- shapiro.test(datosBothUC$Time)

###############################################################
###   Analisis de Varianza  -AOV                            ### 
###############################################################

# AOV de caso de estudio FMSG
MSG.aov <- aov(Time ~ ArtifactsSet, data = datosMSG)

# Summary of the analysis
FileSummaryAoVMSG = summary(MSG.aov)

# AOV de caso de estudio PS
PS.aov <- aov(Time ~ ArtifactsSet, data = datosPS)

# Summary of the analysis
FileSummaryAoVPS = summary(PS.aov)

# AOV de datos ambos casos de estudio. 
Both.aov <- aov(Time ~ ArtifactsSet, data = datosBoth)

# Summary of the analysis
FileSummaryAoVBoth = summary(Both.aov)

###############################################################
###   File output creation                                  ### 
###############################################################

# Title
cat("statistical analysis for response time of correct answers (Score = 4y5)", file = "time-S-SW-AoV.doc")

capture.output(FileSummaryMSGB, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryMSGBU, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryMSGUC, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryPSB, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryPSBU, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryPSUC, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryBothB, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryBothBU, file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryBothUC, file = "time-S-SW-AoV.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      Shapiro Wilk test output file.                                                         ----#
#----------------------------------------------------------------------------------------------------#

capture.output(SwMSGB , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwMSGBU , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwMSGUC , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwPSB , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwPSBU , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwPSUC , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwBothB , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwBothBU , file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(SwBothUC , file = "time-S-SW-AoV.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      AnoVa test on MSG, PS, and Both.                                                       ----#
#----------------------------------------------------------------------------------------------------#

cat("Anova Test MSG\n",file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryAoVMSG, file = "time-S-SW-AoV.doc", append = TRUE)

cat("Anova Test PS\n",file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryAoVPS, file = "time-S-SW-AoV.doc", append = TRUE)

cat("Anova Test Both\n",file = "time-S-SW-AoV.doc", append = TRUE)
capture.output(FileSummaryAoVBoth, file = "time-S-SW-AoV.doc", append = TRUE)

