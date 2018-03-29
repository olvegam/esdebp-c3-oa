# Set path to Desktop

# Para MAC
#setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/Time")

# Para Windows
setwd("D:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/Time")

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

# Lea los datos para los caso de estudio FMSG y PdV.
datosFandP <- read_excel("CR-TimeSc5.xlsx", sheet = 3)
# Lea los datos para el caso de estudio PdV.
#datosPdV <- read_excel("CR-Scored.xlsx", sheet = 8)
# Lea los datos unidos indendiente del caso de estudio. 
datosTwoCS <- read_excel("CR-TimeSc5.xlsx", sheet = 4)
# Lea los datos para ambos casos de estudio, distribuciones B and C.
#datosTwoCSBandC <- read_excel("CR-Subjects.xlsx", sheet = 3)


###############################################################
###   Mostrar una porcion aleatoria de cada conjunto de datos.# 
###############################################################

# Show first a random sample of each data set. 
set.seed(1234)

# Summary of the data 
dplyr::sample_n(datosFandP, 10)
summary(datosFandP)

# Summary of the data 
#dplyr::sample_n(datosPdV, 10)
#summary(datosPdV)

# Summary of the data 
dplyr::sample_n(datosTwoCS, 10)
summary(datosTwoCS)

# Summary of the data 
#dplyr::sample_n(datosTwoCS, 10)
#summary(datosTwoCSBandC)
###############################################################
###   Organice los niveles de los datos.                    ### 
###############################################################

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos FMSG
datosFandP$ArtifactsSet <- ordered(datosFandP$ArtifactsSet, levels(factor(datosFandP$ArtifactsSet)))
#show the levels
levels(datosFandP$ArtifactsSet)

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos PdV
#datosPdV$ArtifactsSet <- ordered(datosPdV$ArtifactsSet, levels(factor(datosPdV$ArtifactsSet)))
#show the levels
#levels(datosPdV$ArtifactsSet)

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos TwoCS
datosTwoCS$ArtifactsSet <- ordered(datosTwoCS$ArtifactsSet, levels(factor(datosTwoCS$ArtifactsSet)))
#show the levels
levels(datosTwoCS$ArtifactsSet)

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos TwoCSBandC
#datosTwoCSBandC$ArtifactsSet <- ordered(datosTwoCSBandC$ArtifactsSet, levels(factor(datosTwoCSBandC$ArtifactsSet)))

#show the levels
#levels(datosTwoCSBandC$ArtifactsSet)

###############################################################
###   Muestre los boxplot de cada conjunto de datos.        ### 
###############################################################

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("BoxPlot-FandP-TRta.eps", onefile=FALSE, height=8, width=10, pointsize=10)

#Dibuje el boxplot con datos de FMSG y PdV (Diagrama de cajas)
bpFandP = ggplot(datosFandP, aes(ArtifactsSet,TRta)) + geom_boxplot(outlier.shape = 19) +
  ggtitle ("Time Response of correct answers -  by case study (P=PdV, F=FMSG)") +
  labs(x = "Artifacts set", y= "Tiempo de Respuesta") + 
  theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.x = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.minor.y = element_line(colour = "grey50"))

#Expórtelo al tipo de archivo preparado
print(bpFandP)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "BoxPlot-FandP-TRta.png", width = 1800, height = 1200, units = "px", 
     res = 200)

#Expórtelo al tipo de archivo preparado
print(bpFandP)
dev.off()

#Exportar la imagen a .tif - prepare el archivo. 
#tiff(filename = "BoxPlot-FMSG-TRta.tif", width = 1800, height = 1200, units = "px", 
#    compression = "lzw", res = 200)

#Exportar la imagen a eps - prepare el archivo. 
#setEPS()
#postscript("BoxPlot-PdV-TRta.eps", onefile=FALSE, height=6, width=8, pointsize=10)

#Exportar la imagen a png - prepare el archivo. 
#png(filename = "BoxPlot-PdV-TRta.png", width = 1800, height = 1200, units = "px", 
#     res = 200)

#Exportar la imagen a .tif - prepare el archivo. 
#tiff(filename = "BoxPlot-PdV-TRta.tif", width = 1800, height = 1200, units = "px", 
#    compression = "lzw", res = 200)

#Dibuje el boxplot de PdV. (Diagrama de cajas)
#ggplot(datosPdV, aes(ArtifactsSet,TRta)) + geom_boxplot() +
#  ggtitle ("Case study point of sale") + 
#  labs(x = "Artifacts set", y= "TRta") + 
#  theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
#  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
#  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
#  theme(axis.text.x = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
#  theme(axis.text.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
#  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.minor.y = element_line(colour = "grey50"))

#Exportar la imagen a png - genere el archivo. 
#dev.off()

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("BoxPlot-TwoCS-TRta.eps", onefile=FALSE, height=8, width=10, pointsize=10)

#Dibuje el boxplot de ambos casos de estudio. (Diagrama de cajas)
bpTwoCS = ggplot(datosTwoCS, aes(ArtifactsSet,TRta)) + geom_boxplot()+
  ggtitle ("Time Response of correct answers - All case studies") +
  labs(x = "Artifacts set", y= "Tiempo de Respuesta") + 
  theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
  theme(axis.title.x = element_text(face="bold", vjust=-0.5, size=rel(1.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.x = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(axis.text.y = element_text(face="bold", vjust=1.5, size=rel(1.5))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(panel.grid.minor.y = element_line(colour = "grey50"))

#Expórtelo al tipo de archivo preparado
print(bpTwoCS)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "BoxPlot-TwoCS-TRta.png", width = 1800, height = 1200, units = "px", 
     res = 200)
print(bpTwoCS)
dev.off()

#Exportar la imagen a .tif - prepare el archivo. 
#tiff(filename = "BoxPlot-TwoCS-TRta.tif", width = 1800, height = 1200, units = "px", 
#    compression = "lzw", res = 200)
#print(bpTwoCS)
#dev.off()


###############################################################
###   Analisis de Varianza  -AOV                            ### 
###############################################################

# AOV de caso de estudio FMSG
FandP.aov <- aov(TRta ~ ArtifactsSet, data = datosFandP)

# Summary of the analysis
FileSummaryFandP = summary(FandP.aov)

# AOV de caso de estudio PdV
#PdV.aov <- aov(TRta ~ ArtifactsSet, data = datosPdV)

# Summary of the analysis
#FileSummaryPdV = summary(PdV.aov)

# AOV de datos ambos casos de estudio. 
TwoCS.aov <- aov(TRta ~ ArtifactsSet, data = datosTwoCS)

# Summary of the analysis
FileSummaryTwoCS = summary(TwoCS.aov)

# AOV de distribucion ArtifactsSet B and C. 
#TwoCSBandC.aov <- aov(TRta ~ ArtifactsSet, data = datosTwoCSBandC)

# Summary of the analysis
#FileSummaryTwoCSBandC = summary(TwoCSBandC.aov)
###############################################################
###   File output creation                                  ### 
###############################################################

# Title
cat("Anova summaries response time of correct answers (Score = 5)", file = "TRta-Aov-Summ.doc")

# add 2 newlines
cat("\n\n", file = "TRta-Aov-Summ.doc", append = TRUE)
# export anova test FMSG output
cat("Anova Test FandP\n", file = "TRta-Aov-Summ.doc", append = TRUE)
capture.output(FileSummaryFandP, file = "TRta-Aov-Summ.doc", append = TRUE)

# add 2 newlines
#cat("\n\n", file = "TRta-Aov-Summ.doc", append = TRUE)
# export anova test PdV output
#cat("Anova Test PdV\n", file = "TRta-Aov-Summ.doc", append = TRUE)
#capture.output(FileSummaryPdV, file = "TRta-Aov-Summ.doc", append = TRUE)

# add 2 newlines
cat("\n\n", file = "TRta-Aov-Summ.doc", append = TRUE)
# export anova test TwoCS output
cat("Anova Test TwoCS\n", file = "TRta-Aov-Summ.doc", append = TRUE)
capture.output(FileSummaryTwoCS, file = "TRta-Aov-Summ.doc", append = TRUE)

# add 2 newlines
#cat("\n\n", file = "TRta-Aov-Summ.doc", append = TRUE)
# export anova test TwoCS output
#cat("Anova Test TwoCS for B and C only \n", file = "TRta-Aov-Summ.doc", append = TRUE)
#capture.output(FileSummaryTwoCSBandC, file = "TRta-Aov-Summ.doc", append = TRUE)
