# Set path to Desktop

# Para MAC
#setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA//1. nCA/numCAsc4y5")

# Para Windows Bogot? - port?til
setwd("D:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/1. nCA/numCAsc4y5")

# Para Windows Unillanos
#setwd("C:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/1. nCA/numCAsc4y5")

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

# Lea los datos de cada distribuci?n por grupo de artefactos para ks test. 
datosMSGB <- read_excel("numCAsc4y5.xlsx", sheet = 5)
datosMSGBU <- read_excel("numCAsc4y5.xlsx", sheet = 6)
datosMSGUC <- read_excel("numCAsc4y5.xlsx", sheet = 7)
datosPSB <- read_excel("numCAsc4y5.xlsx", sheet = 8)
datosPSBU <- read_excel("numCAsc4y5.xlsx", sheet = 9)
datosPSUC <- read_excel("numCAsc4y5.xlsx", sheet = 10)
datosBothB <- read_excel("numCAsc4y5.xlsx", sheet = 11)
datosBothBU <- read_excel("numCAsc4y5.xlsx", sheet = 12)
datosBothUC <- read_excel("numCAsc4y5.xlsx", sheet = 13)

# Lea los datos para el caso de estudio FMSG.
datosFMSG <- read_excel("numCAsc4y5.xlsx", sheet = 2)
# Lea los datos para el caso de estudio PdV.
datosPdV <- read_excel("numCAsc4y5.xlsx", sheet = 3)
# Lea los datos para ambos casos de estudio.
datosTwoCS <- read_excel("numCAsc4y5.xlsx", sheet = 1)


###############################################################
###   Mostrar una porcion aleatoria de cada conjunto de datos.# 
###############################################################

# Show first a random sample of each data set. 
set.seed(1234)

# Show some data 
dplyr::sample_n(datosFMSG, 10)

# Show some data 
dplyr::sample_n(datosPdV, 10)

# Show some data 
dplyr::sample_n(datosTwoCS, 10)

###############################################################
###   Organice los niveles de los datos.                    ### 
###############################################################

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos FMSG
datosFMSG$ArtifactsSet <- ordered(datosFMSG$ArtifactsSet, levels(factor(datosFMSG$ArtifactsSet)))
#show the levels
levels(datosFMSG$ArtifactsSet)

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos PdV
datosPdV$ArtifactsSet <- ordered(datosPdV$ArtifactsSet, levels(factor(datosPdV$ArtifactsSet)))
#show the levels
levels(datosPdV$ArtifactsSet)

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos TwoCS
datosTwoCS$ArtifactsSet <- ordered(datosTwoCS$ArtifactsSet, levels(factor(datosTwoCS$ArtifactsSet)))

#show the levels
levels(datosTwoCS$ArtifactsSet)

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos TwoCS
#datosTwoCSB$ArtifactsSet <- ordered(datosTwoCSB$ArtifactsSet, levels(factor(datosTwoCSB$ArtifactsSet)))

#show the levels
#levels(datosTwoCS$ArtifactsSet)


#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos TwoCSBandC
#datosTwoCSBandC$ArtifactsSet <- ordered(datosTwoCSBandC$ArtifactsSet, levels(factor(datosTwoCSBandC$ArtifactsSet)))

#show the levels
#levels(datosTwoCSBandC$ArtifactsSet)

###############################################################
###   Muestre los boxplot de cada conjunto de datos.        ### 
###############################################################

#Dibuje el boxplot de FMSG (Diagrama de cajas)
bMSG = ggplot(datosFMSG, aes(ArtifactsSet,numCA)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Number of correct answers") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m1-msg.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bMSG)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m1-msg.png", width = 1800, height = 1200, units = "px", 
    res = 200)

#Exp?rtelo al tipo de archivo preparado
print(bMSG)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m1-msg.pdf")
print(bMSG)
dev.off()

#Dibuje el boxplot de PdV. (Diagrama de cajas)
bPdV=ggplot(datosPdV, aes(ArtifactsSet,numCA)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Number of correct answers") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m1-ps.eps", onefile=FALSE, height=8, width=10, pointsize=10)
print(bPdV)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m1-ps.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(bPdV)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m1-ps.pdf")
print(bPdV)
dev.off()

#Dibuje el boxplot de ambos casos de estudio. (Diagrama de cajas)
bTwoCS = ggplot(datosTwoCS, aes(ArtifactsSet,numCA)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Number of correct answers") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
#  theme(panel.grid.minor.y = element_line(colour = "grey50")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m1-both.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bTwoCS)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m1-both.png", width = 1800, height = 1200, units = "px", 
     res = 200)

#Exportar la imagen a png - genere el archivo. 
print(bTwoCS)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m1-both.pdf")
print(bTwoCS)
dev.off()

###############################################################
###   Test de Kolmogorov - Smirnov de las distribuciones    ### 
###############################################################

# MSG-B
KsMSGB<- ks.test(datosMSGB$numCA, "pnorm", mean =datosMSGB$numCA, sd= datosMSGB$numCA)

# MSG-B
KsMSGBU<- ks.test(datosMSGBU$numCA, "pnorm", mean =datosMSGBU$numCA, sd= datosMSGBU$numCA)

# MSG-B
KsMSGUC<- ks.test(datosMSGUC$numCA, "pnorm", mean =datosMSGUC$numCA, sd= datosMSGUC$numCA)

# MSG-B
KsPSB<- ks.test(datosPSB$numCA, "pnorm", mean =datosPSB$numCA, sd= datosPSB$numCA)

# MSG-B
KsPSBU<- ks.test(datosPSBU$numCA, "pnorm", mean =datosPSBU$numCA, sd= datosPSBU$numCA)

# MSG-B
KsPSUC<- ks.test(datosPSUC$numCA, "pnorm", mean =datosPSUC$numCA, sd= datosPSUC$numCA)

# MSG-B
KsBothB<- ks.test(datosBothB$numCA, "pnorm", mean =datosBothB$numCA, sd= datosBothB$numCA)

# MSG-B
KsBothBU<- ks.test(datosBothBU$numCA, "pnorm", mean =datosBothBU$numCA, sd= datosBothBU$numCA)

# MSG-B
KsBothUC<- ks.test(datosBothUC$numCA, "pnorm", mean =datosBothUC$numCA, sd= datosBothUC$numCA)

###############################################################
###   Test de Shapiro Wilk de las distribuciones            ### 
###############################################################

# MSG-B
SwMSGB<- shapiro.test(datosMSGB$numCA)

# MSG-BU
SwMSGBU<- shapiro.test(datosMSGBU$numCA)

# MSG-UC
SwMSGUC<- shapiro.test(datosMSGUC$numCA)

# PS-B
SwPSB<- shapiro.test(datosPSB$numCA)

# PS-BU
SwPSBU<- shapiro.test(datosPSBU$numCA)

# PS-UC
SwPSUC<- shapiro.test(datosPSUC$numCA)

# Both-B
SwBothB<- shapiro.test(datosBothB$numCA)

# Both-BU
SwBothBU<- shapiro.test(datosBothBU$numCA)

# Both-UC
SwBothUC<- shapiro.test(datosBothUC$numCA)

###############################################################
###   Datos de cada distribuci?n - Summary                  ### 
###############################################################

## Summary of the data - MSG 
FileSummaryMSGB = summary(datosMSGB)
FileSummaryMSGBU = summary(datosMSGBU)
FileSummaryMSGUC = summary(datosMSGUC)

## Summary of the data - PdV
FileSummaryPdVB = summary(datosPSB)
FileSummaryPdVBU = summary(datosPSBU)
FileSummaryPdVUC= summary(datosPSUC)

# Summary of the data - BothCS 
FileSummaryBothB = summary(datosBothB)
FileSummaryBothBU = summary(datosBothBU)
FileSummaryBothUC = summary(datosBothUC)

###############################################################
###   Analisis de Varianza  -AOV                            ### 
###############################################################

# AOV de caso de estudio FMSG
#FMSG.aov <- aov(numCA ~ ArtifactsSet, data = datosFMSG)

# Summary of the analysis
#FileSummaryAoVFMSG = summary(FMSG.aov)

# AOV de caso de estudio PdV
#PdV.aov <- aov(numCA ~ ArtifactsSet, data = datosPdV)

# Summary of the analysis
#FileSummaryAoVPdV = summary(PdV.aov)

# AOV de datos ambos casos de estudio. 
#TwoCS.aov <- aov(numCA ~ ArtifactsSet, data = datosTwoCS)

# Summary of the analysis
#FileSummaryAovTwoCS = summary(TwoCS.aov)

###############################################################
###   Analisis of Mann Whitney                              ### 
### Mann-Whitney-Wilcoxon (MWW), Wilcoxon rank-sum test,    ###
### or Wilcoxon-Mann-Whitney test                           ###   
###############################################################

#Man Whitney for MSG
MWMSGBBU = wilcox.test(datosMSGB$numCA, datosMSGBU$numCA)
MWMSGBUUC = wilcox.test(datosMSGBU$numCA, datosMSGUC$numCA)
MWMSGBUC = wilcox.test(datosMSGB$numCA, datosMSGUC$numCA)

#Man Whitney for PS
MWPSBBU = wilcox.test(datosPSB$numCA, datosPSBU$numCA)
MWPSBUUC = wilcox.test(datosPSBU$numCA, datosPSUC$numCA)
MWPSBUC = wilcox.test(datosPSB$numCA, datosPSUC$numCA)

#Man Whitney for Both
MWBothBBU = wilcox.test(datosBothB$numCA, datosBothBU$numCA)
MWBothBUUC = wilcox.test(datosBothBU$numCA, datosBothUC$numCA)
MWBothBUC = wilcox.test(datosBothB$numCA, datosBothUC$numCA)

###############################################################
###   File output creation                                  ### 
###############################################################

# Title
cat("Statistical data for number of correct answers", file = "nCAsc4y5-S-SW-MannW.doc")
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# Case study identification 
cat("MSG Foundation CASE STUDY STATISTICAL ANALYSIS\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# Artifacts Set for case study identification 
cat("MSG-B statistical analysis\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# -------   KS test for MSG-B case study. --------------------------------------------#

# export KS test output for MSG-B 
cat("KS test for MSG-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsMSGB , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# -------   SW test for MSG-B case study. --------------------------------------------#

cat("SW test for MSG-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwMSGB , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# --------  Summary of MSG-B distribution -------------------------------------------#

cat("Summary MSG-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGB, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export KS test output for MSG-BU
cat("KS test for MSG-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsMSGBU , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for MSG-BU
cat("SW test for MSG-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwMSGBU , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export KS test output for MSG-UC
cat("KS test for MSG-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsMSGUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for MSG-UC
cat("SW test for MSG-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwMSGUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export anova test FMSG output
#cat("Anova Test FMSG\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
#capture.output(FileSummaryAoVFMSG, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#
# -------   Mann Whitney U test to pdf file output - MSG case study-------------------------#
# ------------------------------------------------------------------------------------------#

# export Mann Whitney U test output for MSG-B
cat("Man Whitney U test for MSG-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWMSGBBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export Mann Whitney U test output for MSG-B
cat("Man Whitney U test for MSG-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWMSGBUUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export Mann Whitney U test output for MSG-B
cat("Man Whitney U test for MSG-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWMSGBUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# Case study identification 
cat("Point of Sale CASE STUDY STATISTICAL ANALYSIS\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# Artifacts set - Case study identification 
cat("PS-B statistical analysis\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# export KS test output for PdV-B
cat("KS test for PS-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsPSB, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for PdV-B
cat("SW test for PS-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwPSB, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-BV\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVB, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# Artifacts Set for case study identification 
cat("MSG-BU statistical analysis\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# export KS test output for PdV-BU
cat("KS test for PS-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsPSBU , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for PdV-BU
cat("SW test for PS-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwPSBU , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary for PS-BU 
cat("Summary PS-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# Artifacts Set for case study identification 
cat("MSG-UC statistical analysis\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# export KS test output for PS-UC
cat("KS test for PS-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsPSUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#

# export SW test output for PS-UC
cat("SW test for PS-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwPSUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export anova test PdV output
# cat("Anova Test PdV\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# capture.output(FileSummaryAoVPdV, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
# cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#
# -------   Mann Whitney U test to pdf file output - PS case study--------------------------#
# ------------------------------------------------------------------------------------------#

# export Mann Whitney U test output for PS-B
cat("Man Whitney U test for PS-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWPSBBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export Mann Whitney U test output for PS-BU
cat("Man Whitney U test for PS-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWPSBUUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export Mann Whitney U test output for PS-UC
cat("Man Whitney U test for PS-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWPSBUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# -------------------------------------------------------------------------------------#

# Case study identification 
cat("BOTH CASE STUDIES STATISTICAL ANALYSIS\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# -------   KS test for Both case studies. --------------------------------------------#

# export KS test output for Both-B
cat("KS test for Both-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsBothB , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for Both-B
cat("SW test for Both-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwBothB , file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary Both output
cat("Summary Both-B\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryBothB, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export KS test output for Both-BU
cat("KS test for Both-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsBothBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for Both-BU
cat("SW test for Both-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwBothBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary Both output
cat("Summary Both-BU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryBothBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export KS test output for Both-UC
cat("KS test for Both-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(KsBothUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export SW test output for Both-UC
cat("SW test for Both-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(SwBothUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export summary Both output
cat("Summary Both-UC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(FileSummaryBothUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export anova test TwoCS output
# cat("Anova Test Both\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# capture.output(FileSummaryAovTwoCS, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# ------------------------------------------------------------------------------------------#
# -------   Mann Whitney U test to pdf file output - Both case study--------------------------#
# ------------------------------------------------------------------------------------------#

# export Mann Whitney U test output for Both-B
cat("Man Whitney U test for Both-BvsBU\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWBothBBU, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export Mann Whitney U test output for PS-BU
cat("Man Whitney U test for Both-BUvsUC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWBothBUUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)

# export Mann Whitney U test output for PS-UC
cat("Man Whitney U test for Both-BvsUC\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
capture.output(MWBothBUC, file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nCAsc4y5-S-SW-MannW.doc", append = TRUE)


