# Set path to Desktop

# Para MAC
# setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/5. Time")

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

#Para Cliff's delta
#  install.packages("effsize")

##############################################################
###   Leer los datos (cada caso y los de ambos casos)      ###
##############################################################

# Lea los datos para comparaci?n de distribuciones. 
datosBoth <- read_excel("CR-TimeSc4y5.xlsx", sheet = 1)
datosMSG <- read_excel("CR-TimeSc4y5.xlsx", sheet = 5)
datosPS <- read_excel("CR-TimeSc4y5.xlsx", sheet = 9)

# Lea los datos de cada distribuci?n por grupo de artefactos para SW test. 

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
bMSG = ggplot(datosMSG, aes(ArtifactsSet,Time)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  labs(x = "Artifacts set", y= "Answering time") + 
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")


#  geom_boxplot(outlier.shape = 19, width = 0.3, position = position_dodge(0.5)) +
# Para que el box salga acostado 
#       coord_flip () +
# ---otros parámetros para ggplot: 
# Para el título: 
# - tamaño y justificación
#       theme (plot.title = element_text(vjust=2, hjust=0.5, lineheight=1.5, size = 20))  +
# Para el fondo del plano, 
# - que tenga líneas horizontales
#       theme(panel.grid.minor.y = element_line(colour = "grey50"))
# Par los ejes
# - tamaño y texto de los títulos y las etiquetas de los ejes. 
# theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

#Exportar la imagen a eps - prepare el archivo. 

setEPS()
postscript("rq2-m4-msg.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bMSG)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq2-m4-msg.png", width = 1800, height = 1200, units = "px", 
     res = 200)
print(bMSG)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq2-m4-msg.pdf")
print(bMSG)
dev.off()

#Dibuje el boxplot de PS. (Diagrama de cajas)
bPS = ggplot(datosPS, aes(ArtifactsSet,Time)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  labs(x = "Artifacts set", y= "Answering time") + 
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq2-m4-ps.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bPS)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq2-m4-ps.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(bPS)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq2-m4-ps.pdf")
print(bPS)
dev.off()

#Dibuje el boxplot de ambos casos de estudio. (Diagrama de cajas)
bBoth = ggplot(datosBoth, aes(ArtifactsSet,Time)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  labs(x = "Artifacts set", y= "Answering time") + 
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18, size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq2-m4-both.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bBoth)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq2-m4-both.png", width = 1800, height = 1200, units = "px", 
     res = 200)
print(bBoth)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq2-m4-both.pdf")
print(bBoth)
dev.off()

###############################################################
###   Datos de cada distribuci?n - Summary                  ### 
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
###   Test de Kolmogorov - Smirnov                          ### 
###############################################################

KsMSGB<- ks.test(datosMSGB$Time, "pnorm", mean =datosMSGB$Time, sd= datosMSGB$Time)
KsMSGBU<- ks.test(datosMSGBU$Time, "pnorm", mean =datosMSGBU$Time, sd= datosMSGBU$Time)
KsMSGUC<- ks.test(datosMSGUC$Time, "pnorm", mean =datosMSGUC$Time, sd= datosMSGUC$Time)

KsPSB<- ks.test(datosPSB$Time, "pnorm", mean =datosPSB$Time, sd= datosPSB$Time)
KsPSBU<- ks.test(datosPSBU$Time, "pnorm", mean =datosPSBU$Time, sd= datosPSBU$Time)
KsPSUC<- ks.test(datosPSUC$Time, "pnorm", mean =datosPSUC$Time, sd= datosPSUC$Time)

KsBothB<- ks.test(datosBothB$Time, "pnorm", mean =datosBothB$Time, sd= datosBothB$Time)
KsBothBU<- ks.test(datosBothBU$Time, "pnorm", mean =datosBothBU$Time, sd= datosBothBU$Time)
KsBothUC<- ks.test(datosBothUC$Time, "pnorm", mean =datosBothUC$Time, sd= datosBothUC$Time)

###############################################################
###   Mann Whitney Test                                     ### 
### Mann-Whitney-Wilcoxon (MWW), Wilcoxon rank-sum test,    ###
### or Wilcoxon-Mann-Whitney test                           ###   
###############################################################

#Man Whitney for MSG
MWMSGBBU = wilcox.test(datosMSGB$Time, datosMSGBU$Time)
MWMSGBUUC = wilcox.test(datosMSGBU$Time, datosMSGUC$Time)
MWMSGBUC = wilcox.test(datosMSGB$Time, datosMSGUC$Time)

#Man Whitney for PS
MWPSBBU = wilcox.test(datosPSB$Time, datosPSBU$Time)
MWPSBUUC = wilcox.test(datosPSBU$Time, datosPSUC$Time)
MWPSBUC = wilcox.test(datosPSB$Time, datosPSUC$Time)

#Man Whitney for Both
MWBothBBU = wilcox.test(datosBothB$Time, datosBothBU$Time)
MWBothBUUC = wilcox.test(datosBothBU$Time, datosBothUC$Time)
MWBothBUC = wilcox.test(datosBothB$Time, datosBothUC$Time)

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
cat("statistical analysis for response time of correct answers (Score = 4y5)", file = "time-S-SW-AoV-KS-MannW.doc")

#----------------------------------------------------------------------------------------------------#
#---      Summary of the samples.                                                                ----#
#----------------------------------------------------------------------------------------------------#

cat("MSG-B", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("MSG-BU", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGBU, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("MSG-UC", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("PS-B", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryPSB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("PS-BU", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryPSBU, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("PS-UC", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryPSUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("Both-B", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryBothB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("Both-BU", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryBothBU, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("Both-UC", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryBothUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      Shapiro Wilk test output file.                                                         ----#
#----------------------------------------------------------------------------------------------------#

capture.output(SwMSGB , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwMSGBU , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwMSGUC , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwPSB , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwPSBU , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwPSUC , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwBothB , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwBothBU , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(SwBothUC , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      AnoVa test on MSG, PS, and Both.                                                       ----#
#----------------------------------------------------------------------------------------------------#

cat("Anova Test MSG\n",file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryAoVMSG, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("Anova Test PS\n",file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryAoVPS, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

cat("Anova Test Both\n",file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(FileSummaryAoVBoth, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      Kolmogorov test output file.                                                           ----#
#----------------------------------------------------------------------------------------------------#

capture.output(KsMSGB , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsMSGBU , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsMSGUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsPSB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsPSBU , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsPSUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsBothB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsBothBU , file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(KsBothUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#
# -------   Mann Whitney U test to pdf file output - MSG case study-------------------------#
# ------------------------------------------------------------------------------------------#

# export Mann Whitney U test output for MSG-B
capture.output(MWMSGBBU, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(MWMSGBUUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(MWMSGBUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

capture.output(MWPSBBU, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(MWPSBUUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(MWPSBUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

capture.output(MWBothBBU, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(MWBothBUUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(MWBothBUC, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#
#  Cliff's delta for the samples with significative difference with Bonferroni corection  --#
# ------------------------------------------------------------------------------------------#

# ------------------------------------------------------------------------------------------#
#  Cliff's delta for the samples with significative difference with Bonferroni corection  --#
# ------------------------------------------------------------------------------------------#

CdPSBUvsB <- effsize::cliff.delta(datosPSBU$Time, datosPSB$Time, use.unbiased = TRUE, conf.level = .95)
CdPSUCvsB <- effsize::cliff.delta(datosPSUC$Time, datosPSB$Time, use.unbiased = TRUE, conf.level = .95)
CdBothUCvsB <- effsize::cliff.delta(datosBothUC$Time, datosBothB$Time, use.unbiased = TRUE, conf.level = .95)

cat("PS-BU vs PS-B", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(CdPSBUvsB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
cat("PS-UC vs PS-B", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(CdPSUCvsB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
cat("Both-UC vs Both-B", file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
capture.output(CdBothUCvsB, file = "time-S-SW-AoV-KS-MannW.doc", append = TRUE)
