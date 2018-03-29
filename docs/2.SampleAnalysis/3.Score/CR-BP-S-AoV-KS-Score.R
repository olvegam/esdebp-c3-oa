# Set path to Desktop

# Para MAC
#setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/3. Score")

# Para Windows
setwd("D:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/3. Score")

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

# Lea los datos de cada distribución por grupo de artefactos para ks test. 
datosMSGB <- read_excel("CR-Score.xlsx", sheet = 3)
datosMSGBU <- read_excel("CR-Score.xlsx", sheet = 4)
datosMSGUC <- read_excel("CR-Score.xlsx", sheet = 5)
datosPSB <- read_excel("CR-Score.xlsx", sheet = 7)
datosPSBU <- read_excel("CR-Score.xlsx", sheet = 8)
datosPSUC <- read_excel("CR-Score.xlsx", sheet = 9)

datosBothB <- read_excel("CR-Score.xlsx", sheet = 11)
datosBothBU <- read_excel("CR-Score.xlsx", sheet = 12)
datosBothUC <- read_excel("CR-Score.xlsx", sheet = 13)

# Lea los datos para el caso de estudio FMSG.
datosFMSG <- read_excel("CR-Score.xlsx", sheet = 2)
# Lea los datos para el caso de estudio PdV.
datosPdV <- read_excel("CR-Score.xlsx", sheet = 6)
# Lea los datos para ambos casos de estudio.
datosBoth <- read_excel("CR-Score.xlsx", sheet = 10)


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
dplyr::sample_n(datosBoth, 10)

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

#Asigne los niveles a la columna que los tiene (en este caso ArtifactsSet) - datos Both
datosBoth$ArtifactsSet <- ordered(datosBoth$ArtifactsSet, levels(factor(datosBoth$ArtifactsSet)))
#show the levels
levels(datosBoth$ArtifactsSet)

###############################################################
###   Dibuje los boxplot de cada conjunto de datos.        ### 
###############################################################

#Dibuje el boxplot de FMSG (Diagrama de cajas)

bMSG = ggplot(datosFMSG, aes(ArtifactsSet,AvgScore)) + 
  geom_boxplot() +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Average Score") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.margin = unit(c(6,1,6,1), "cm")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")+
  coord_flip(expand = TRUE)


#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m3-msg-horizontal.pdf")
print(bMSG)
dev.off()

bMSG = ggplot(datosFMSG, aes(ArtifactsSet,AvgScore)) + 
  geom_boxplot() +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  theme(axis.text.x = element_text(face="bold", angle = 90, hjust=0.5, 
                                   size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Average Score") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(plot.margin = unit(c(1,6,1,6), "cm")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")+
#  coord_flip(expand = TRUE)

  #Exportar la imagen a pdf - prepare el archivo
  pdf("rq1-m3-msg-Vertical-labelToo.pdf")
  print(bMSG)
  dev.off()




#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m3-msg.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bMSG)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m3-msg.png", width = 1800, height = 1200, units = "px", 
    res = 200)

#Exp?rtelo al tipo de archivo preparado
print(bMSG)
dev.off()

#Dibuje el boxplot de PdV. (Diagrama de cajas)
bPdV=ggplot(datosPdV, aes(ArtifactsSet,AvgScore)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Average Score") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m3-ps.eps", onefile=FALSE, height=8, width=10, pointsize=10)
print(bPdV)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m3-ps.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(bPdV)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m3-ps.pdf")
print(bPdV)
dev.off()

#Dibuje el boxplot de ambos casos de estudio. (Diagrama de cajas)
bBoth = ggplot(datosBoth, aes(ArtifactsSet,AvgScore)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Average Score") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m3-both.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bBoth)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m3-both.png", width = 1800, height = 1200, units = "px", 
     res = 200)

#Exportar la imagen a png - genere el archivo. 
print(bBoth)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m3-both.pdf")
print(bBoth)
dev.off()

###############################################################
###   Test de Kolmogorov - Smirnov de las distribuciones    ### 
###############################################################

# MSG-B
KsMSGB<- ks.test(datosMSGB$AvgScore, "pnorm", mean =datosMSGB$AvgScore, sd= datosMSGB$AvgScore)

# MSG-B
KsMSGBU<- ks.test(datosMSGBU$AvgScore, "pnorm", mean =datosMSGBU$AvgScore, sd= datosMSGBU$AvgScore)

# MSG-B
KsMSGUC<- ks.test(datosMSGUC$AvgScore, "pnorm", mean =datosMSGUC$AvgScore, sd= datosMSGUC$AvgScore)

# MSG-B
KsPSB<- ks.test(datosPSB$AvgScore, "pnorm", mean =datosPSB$AvgScore, sd= datosPSB$AvgScore)

# MSG-B
KsPSBU<- ks.test(datosPSBU$AvgScore, "pnorm", mean =datosPSBU$AvgScore, sd= datosPSBU$AvgScore)

# MSG-B
KsPSUC<- ks.test(datosPSUC$AvgScore, "pnorm", mean =datosPSUC$AvgScore, sd= datosPSUC$AvgScore)


# MSG-B
KsBothB<- ks.test(datosBothB$AvgScore, "pnorm", mean =datosBothB$AvgScore, sd= datosBothB$AvgScore)

# MSG-B
KsBothBU<- ks.test(datosBothBU$AvgScore, "pnorm", mean =datosBothBU$AvgScore, sd= datosBothBU$AvgScore)

# MSG-B
KsBothUC<- ks.test(datosBothUC$AvgScore, "pnorm", mean =datosBothUC$AvgScore, sd= datosBothUC$AvgScore)

###############################################################
###   Datos de cada distribución - Summary                  ### 
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
###   Test de Shapiro Wilk                                  ### 
###############################################################

SwMSGB<- shapiro.test(datosMSGB$AvgScore)
SwMSGBU<- shapiro.test(datosMSGBU$AvgScore)
SwMSGUC<- shapiro.test(datosMSGUC$AvgScore)
SwPSB<- shapiro.test(datosPSB$AvgScore)
SwPSBU<- shapiro.test(datosPSBU$AvgScore)
SwPSUC<- shapiro.test(datosPSUC$AvgScore)
SwBothB<- shapiro.test(datosBothB$AvgScore)
SwBothBU<- shapiro.test(datosBothBU$AvgScore)
SwBothUC<- shapiro.test(datosBothUC$AvgScore)

###############################################################
###   Mann Whitney Test                                     ### 
### Mann-Whitney-Wilcoxon (MWW), Wilcoxon rank-sum test,    ###
### or Wilcoxon-Mann-Whitney test                           ###   
###############################################################

#Man Whitney for MSG
MWMSGBBU = wilcox.test(datosMSGB$AvgScore, datosMSGBU$AvgScore)
MWMSGBUUC = wilcox.test(datosMSGBU$AvgScore, datosMSGUC$AvgScore)
MWMSGBUC = wilcox.test(datosMSGB$AvgScore, datosMSGUC$AvgScore)

#Man Whitney for PS
MWPSBBU = wilcox.test(datosPSB$AvgScore, datosPSBU$AvgScore)
MWPSBUUC = wilcox.test(datosPSBU$AvgScore, datosPSUC$AvgScore)
MWPSBUC = wilcox.test(datosPSB$AvgScore, datosPSUC$AvgScore)

#Man Whitney for Both
MWBothBBU = wilcox.test(datosBothB$AvgScore, datosBothBU$AvgScore)
MWBothBUUC = wilcox.test(datosBothBU$AvgScore, datosBothUC$AvgScore)
MWBothBUC = wilcox.test(datosBothB$AvgScore, datosBothUC$AvgScore)


###############################################################
###   Analisis de Varianza  -AOV                            ### 
###############################################################

# AOV de caso de estudio FMSG
FMSG.aov <- aov(AvgScore ~ ArtifactsSet, data = datosFMSG)

# Summary of the analysis
FileSummaryAoVFMSG = summary(FMSG.aov)

# AOV de caso de estudio PdV
PdV.aov <- aov(AvgScore ~ ArtifactsSet, data = datosPdV)

# Summary of the analysis
FileSummaryAoVPdV = summary(PdV.aov)

# AOV de datos ambos casos de estudio. 
Both.aov <- aov(AvgScore ~ ArtifactsSet, data = datosBoth)

# Summary of the analysis
FileSummaryAovBoth = summary(Both.aov)

###############################################################
###   File output creation                                  ### 
###############################################################

# Title
cat("Statistical data for average score of the 11 requirements comprehension questions", file = "AvgScore-S-KS-SW-AoV-MannW.doc")
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-B\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGB, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-BU\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-UC\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-BV\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVB, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-UC\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for MSG-B 
cat("KS test for MSG-B\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsMSGB , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for MSG-BU
cat("KS test for MSG-BU\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsMSGBU , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for MSG-UC
cat("KS test for MSG-UC\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsMSGUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for PdV-B
cat("KS test for PS-B\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsPSB, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for PdV-BU
cat("KS test for PS-BU\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsPSBU , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-BU\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for PdV-UC
cat("KS test for PS-UC\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsPSUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for Both-B
#cat("KS test for Both-B\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(KsBothB , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary Both output
#cat("Summary Both-B\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(FileSummaryBothB, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for Both-BU
#cat("KS test for Both-BU\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(KsBothBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary Both output
#cat("Summary Both-BU\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(FileSummaryBothBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for Both-UC
#cat("KS test for Both-UC\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(KsBothUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary Both output
#cat("Summary Both-UC\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(FileSummaryBothUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

# add 2 newlines
#cat("\n\n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
# export anova test Both output
#cat("Anova Test Both for B and C only \n", file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
#capture.output(FileSummaryAovBothBandC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      Shapiro Wilk test output file.                                                         ----#
#----------------------------------------------------------------------------------------------------#

capture.output(SwMSGB , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwMSGBU , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwMSGUC , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwPSB , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwPSBU , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwPSUC , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwBothB , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwBothBU , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwBothUC , file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)


# ------------------------------------------------------------------------------------------#
# -------   Anova test output file                                                   -------#
# ------------------------------------------------------------------------------------------#

capture.output(FileSummaryAoVFMSG, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryAoVPdV, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryAovBoth, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)


# ------------------------------------------------------------------------------------------#
# -------   Mann Whitney U test to pdf file output - MSG case study-------------------------#
# ------------------------------------------------------------------------------------------#

# export Mann Whitney U test output for MSG-B
capture.output(MWMSGBBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWMSGBUUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWMSGBUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

capture.output(MWPSBBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWPSBUUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWPSBUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)

capture.output(MWBothBBU, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWBothBUUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWBothBUC, file = "AvgScore-S-KS-SW-AoV-MannW.doc", append = TRUE)



