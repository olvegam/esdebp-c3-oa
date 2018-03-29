# Set path to Desktop

# Para MAC
#setwd("~/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/2. Subjects/ThirdAn")

# Para Windows
setwd("D:/proys/0-olvegamphd/1. Research/Experiments/exdebp-exp/SA/2. Subjects/ThirdAn")

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
datosMSGB <- read_excel("CR-Subjects.xlsx", sheet = 2)
datosMSGBU <- read_excel("CR-Subjects.xlsx", sheet = 3)
datosMSGUC <- read_excel("CR-Subjects.xlsx", sheet = 4)
datosPSB <- read_excel("CR-Subjects.xlsx", sheet = 6)
datosPSBU <- read_excel("CR-Subjects.xlsx", sheet = 7)
datosPSUC <- read_excel("CR-Subjects.xlsx", sheet = 8)
datosBothB <- read_excel("CR-Subjects.xlsx", sheet = 11)
datosBothBU <- read_excel("CR-Subjects.xlsx", sheet = 12)
datosBothUC <- read_excel("CR-Subjects.xlsx", sheet = 13)

# Lea los datos para el caso de estudio FMSG.
datosFMSG <- read_excel("CR-Subjects.xlsx", sheet = 1)
# Lea los datos para el caso de estudio PdV.
datosPdV <- read_excel("CR-Subjects.xlsx", sheet = 5)
# Lea los datos para ambos casos de estudio.
datosBoth <- read_excel("CR-Subjects.xlsx", sheet = 10)

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
###   Muestre los boxplot de cada conjunto de datos.        ### 
###############################################################

#Dibuje el boxplot de FMSG (Diagrama de cajas)
bMSG = ggplot(datosFMSG, aes(ArtifactsSet,Subjects)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Participants") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m2-msg.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bMSG)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m2-msg.png", width = 1800, height = 1200, units = "px", 
    res = 200)

#Exp?rtelo al tipo de archivo preparado
print(bMSG)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m2-msg.pdf")
print(bMSG)
dev.off()

#Dibuje el boxplot de PdV. (Diagrama de cajas)
bPdV=ggplot(datosPdV, aes(ArtifactsSet,Subjects)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Participants") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m2-ps.eps", onefile=FALSE, height=8, width=10, pointsize=10)
print(bPdV)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m2-ps.png", width = 1800, height = 1200, units = "px", 
    res = 200)
print(bPdV)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m2-ps.pdf")
print(bPdV)
dev.off()

#Dibuje el boxplot de ambos casos de estudio. (Diagrama de cajas)
bBoth = ggplot(datosBoth, aes(ArtifactsSet,Subjects)) + 
  geom_boxplot(outlier.shape = 19, width = 0.4, position = "dodge") +
  scale_x_discrete(expand=c(0.4,0)) +
  theme(axis.title.x = element_text(face="bold", hjust=0.5, size=rel(2.5))) +
  theme(axis.title.y = element_text(face="bold", vjust=0.5, size=rel(2.5))) +
  theme(axis.text.x = element_text(face="bold", hjust=0.5, size=rel(2))) +
  theme(axis.text.y = element_text(face="bold", vjust=0.5, size=rel(2))) +
  labs(x = "Artifacts set", y= "Participants") + 
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  stat_summary(fun.y=mean, geom="point", shape=18,
               size=3, color="red")

#Exportar la imagen a eps - prepare el archivo. 
setEPS()
postscript("rq1-m2-both.eps", onefile=FALSE, height=6, width=8, pointsize=10)
print(bBoth)
dev.off()

#Exportar la imagen a png - prepare el archivo. 
png(filename = "rq1-m2-both.png", width = 1800, height = 1200, units = "px", 
     res = 200)

#Exportar la imagen a png - genere el archivo. 
print(bBoth)
dev.off()

#Exportar la imagen a pdf - prepare el archivo
pdf("rq1-m2-both.pdf")
print(bBoth)
dev.off()

###############################################################
###   Test de Kolmogorov - Smirnov de las distribuciones    ### 
###############################################################

# MSG-B
KsMSGB<- ks.test(datosMSGB$Subjects, "pnorm", mean =datosMSGB$Subjects, sd= datosMSGB$Subjects)

# MSG-B
KsMSGBU<- ks.test(datosMSGBU$Subjects, "pnorm", mean =datosMSGBU$Subjects, sd= datosMSGBU$Subjects)

# MSG-B
KsMSGUC<- ks.test(datosMSGUC$Subjects, "pnorm", mean =datosMSGUC$Subjects, sd= datosMSGUC$Subjects)

# MSG-B
KsPSB<- ks.test(datosPSB$Subjects, "pnorm", mean =datosPSB$Subjects, sd= datosPSB$Subjects)

# MSG-B
KsPSBU<- ks.test(datosPSBU$Subjects, "pnorm", mean =datosPSBU$Subjects, sd= datosPSBU$Subjects)

# MSG-B
KsPSUC<- ks.test(datosPSUC$Subjects, "pnorm", mean =datosPSUC$Subjects, sd= datosPSUC$Subjects)

# MSG-B
KsBothB<- ks.test(datosBothB$Subjects, "pnorm", mean =datosBothB$Subjects, sd= datosBothB$Subjects)

# MSG-B
KsBothBU<- ks.test(datosBothBU$Subjects, "pnorm", mean =datosBothBU$Subjects, sd= datosBothBU$Subjects)

# MSG-B
KsBothUC<- ks.test(datosBothUC$Subjects, "pnorm", mean =datosBothUC$Subjects, sd= datosBothUC$Subjects)

###############################################################
###   Test de Shapiro Wilk                                  ### 
###############################################################

# MSG-B
SwMSGB<- shapiro.test(datosMSGB$Subjects)

# MSG-BU
SwMSGBU<- shapiro.test(datosMSGBU$Subjects)

# MSG-UC
SwMSGUC<- shapiro.test(datosMSGUC$Subjects)

# PS-B
SwPSB<- shapiro.test(datosPSB$Subjects)

# PS-BU
SwPSBU<- shapiro.test(datosPSBU$Subjects)

# PS-UC
SwPSUC<- shapiro.test(datosPSUC$Subjects)

# Both-B
SwBothB<- shapiro.test(datosBothB$Subjects)

# Both-BU
SwBothBU<- shapiro.test(datosBothBU$Subjects)

# Both-UC
SwBothUC<- shapiro.test(datosBothUC$Subjects)

###############################################################
###   Mann Whitney Test                                     ### 
### Mann-Whitney-Wilcoxon (MWW), Wilcoxon rank-sum test,    ###
### or Wilcoxon-Mann-Whitney test                           ###   
###############################################################

#Man Whitney for MSG
MWMSGBBU = wilcox.test(datosMSGB$Subjects, datosMSGBU$Subjects)
MWMSGBUUC = wilcox.test(datosMSGBU$Subjects, datosMSGUC$Subjects)
MWMSGBUC = wilcox.test(datosMSGB$Subjects, datosMSGUC$Subjects)

#Man Whitney for PS
MWPSBBU = wilcox.test(datosPSB$Subjects, datosPSBU$Subjects)
MWPSBUUC = wilcox.test(datosPSBU$Subjects, datosPSUC$Subjects)
MWPSBUC = wilcox.test(datosPSB$Subjects, datosPSUC$Subjects)

#Man Whitney for Both
MWBothBBU = wilcox.test(datosBothB$Subjects, datosBothBU$Subjects)
MWBothBUUC = wilcox.test(datosBothBU$Subjects, datosBothUC$Subjects)
MWBothBUC = wilcox.test(datosBothB$Subjects, datosBothUC$Subjects)

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
FMSG.aov <- aov(Subjects ~ ArtifactsSet, data = datosFMSG)

# Summary of the analysis
FileSummaryAoVFMSG = summary(FMSG.aov)

# AOV de caso de estudio PdV
PdV.aov <- aov(Subjects ~ ArtifactsSet, data = datosPdV)

# Summary of the analysis
FileSummaryAoVPdV = summary(PdV.aov)

# AOV de datos ambos casos de estudio. 
Both.aov <- aov(Subjects ~ ArtifactsSet, data = datosBoth)

# Summary of the analysis
FileSummaryAovBoth = summary(Both.aov)

###############################################################
###   File output creation                                  ### 
###############################################################

# Title
cat("Statistical data for number of subjects with correct answers (Score 4 or 5) ", file = "nSubj-S-KS-SW-AoV-MannW.doc")
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

capture.output(KsMSGB , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

###############################################################
###   Summary of the samples.                               ### 
###############################################################

# export summary FMSG output
cat("Summary MSG-B\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGB, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
cat("\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-BU\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
cat("\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary FMSG output
cat("Summary MSG-UC\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryMSGUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-BV\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVB, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-BU\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary PdV output
cat("Summary PS-UC\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryPdVUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary Both BU output
cat("Summary Both-BU\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryBothBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary Both UC output
cat("Summary Both-UC\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryBothUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)


###############################################################
###   KS test output                                        ### 
###############################################################

# export KS test output for MSG-BU
capture.output(KsMSGBU , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsMSGUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for PdV-B
cat("KS test for PS-B\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsPSB, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for PdV-BU
cat("KS test for PS-BU\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsPSBU , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for PdV-UC
cat("KS test for PS-UC\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsPSUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for Both-B
cat("KS test for Both-B\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsBothB , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export summary Both B output
cat("Summary Both-B\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryBothB, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for Both-BU
cat("KS test for Both-BU\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsBothBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export KS test output for Both-UC
cat("KS test for Both-UC\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(KsBothUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      Shapiro Wilk test output file.                                                         ----#
#----------------------------------------------------------------------------------------------------#

capture.output(SwMSGB , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwMSGBU , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwMSGUC , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwPSB , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwPSBU , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwPSUC , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwBothB , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwBothBU , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(SwBothUC , file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

#----------------------------------------------------------------------------------------------------#
#---      AnoVA test output file.                                                                ----#
#----------------------------------------------------------------------------------------------------#

cat("Anova Test FMSG\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryAoVFMSG, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 1 newline
cat("\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export anova test PdV output
cat("Anova Test PdV\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryAoVPdV, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 1 newline
cat("\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

# export anova test Both output
cat("Anova Test Both\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(FileSummaryAovBoth, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
# add 2 newlines
cat("\n", file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)


# ------------------------------------------------------------------------------------------#
# -------   Mann Whitney U test to pdf file output - MSG case study-------------------------#
# ------------------------------------------------------------------------------------------#

# export Mann Whitney U test output for MSG-B
capture.output(MWMSGBBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWMSGBUUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWMSGBUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

capture.output(MWPSBBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWPSBUUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWPSBUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)

capture.output(MWBothBBU, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWBothBUUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)
capture.output(MWBothBUC, file = "nSubj-S-KS-SW-AoV-MannW.doc", append = TRUE)



