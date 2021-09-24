########################################################################################################
#' @title streamWidthRatingCurve

#' @author Bobby Hensley email: hensley@battelleecology.org

#' @description R script which generates a rating curve of mean stream width versus discharge for
#' NEON sites using field measurements included as part of the reaeration data product (DP1.20190)
#' and the field discharge data product (DP1.20048). Widths are in units of meters and discharge in 
#' units of liters per second. 

########################################################################################################
library(neonUtilities)
library(plyr)
library(ggplot2)

#' Set site ID
siteID=c("HOPB", "LEWI", "POSE", "CUPE",
         "GUIL", "KING", "MCDI", "LECO",
         "WALK", "MAYF", "ARIK", "BLUE",
         "PRIN", "BLDE", "COMO", "WLOU", 
         "SYCA", "REDB", "MART", "MCRA",
         "BIGC", "TECR", "OKSR", "CARI")

#' Pull reaeration data from NEON API into R environment
reaDP<-neonUtilities::loadByProduct(dpID="DP1.20190.001", siteID, check.size = F)
list2env(reaDP,.GlobalEnv)

#' Pull discharge data from NEON API into R environment
dscDP<-neonUtilities::loadByProduct(dpID="DP1.20048.001", siteID, check.size = F)
list2env(dscDP,.GlobalEnv)
discharge<-dsc_fieldData[,c("collectDate","totalDischarge")]

#' Calculates a mean and standard deviation of wetted width for each sampling date 
meanWidths<-plyr::ddply(rea_widthFieldData,c("collectDate", "siteID"),summarise,meanWidth=mean(wettedWidth),sdWidth=sd(wettedWidth))

#' Matches wetted width and discharge data
meanWidths$collectDate<-as.Date(meanWidths$collectDate)
discharge$collectDate<-as.Date(discharge$collectDate)
meanWidths<-merge(meanWidths,discharge,by.x="collectDate",by.y="collectDate",all.x=F,all.y=F)

#' Calculates log of discharge in order to fit a power function w=aQ^b 
#' (see Leopold and Maddock 1953 doi: 10.3133/pp252)
meanWidths$logQ<-log10(meanWidths$totalDischarge)
fittedModel<-lm(meanWidth~logQ,data=meanWidths)
summary(fittedModel)

#' Generates plot of fitted relationship
ggplot(data = meanWidths, aes(x = logQ, y = meanWidth)) + geom_point(color="red",size=5) + ggtitle(siteID) + 
  theme(plot.title=element_text(size=30),axis.title.x = element_text(size=20),axis.text.x = element_text(size=20),
        axis.title.y = element_text(size=20),axis.text.y = element_text(size=20)) +
  xlab("Log Q (L/s)") +  ylab("mean w (m)") + geom_smooth(method="lm",formula="y~x")