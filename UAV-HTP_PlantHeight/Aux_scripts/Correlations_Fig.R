
##Reading all the data and select the specific column of the data (agreetment with curretelly mensuration)
setwd()
BLUEs_GroundPH <- read.csv("rawData1Y_Ground_PH.csv")
str(BLUEs_GroundPH)

library(corrplot)
library(PerformanceAnalytics)

chart.Correlation(as.matrix(BLUEs_GroundPH[2:4]), histogram=TRUE, pch=19)

#In the above plot:

#The distribution of each variable is shown on the diagonal.
#On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed
#On the top of the diagonal : the value of the correlation plus the significance level as stars
#Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols("***", "**", "*", ".", " ")

################2017/2018 - SITE 1 #################
BLUEs_GroundPH2 <- read.csv("rawData2Y_Ground_PH.csv")
str(BLUEs_GroundPH2)

chart.Correlation(as.matrix(BLUEs_GroundPH2[2:5]), histogram=TRUE, pch=19)


################2017/2018 - SITE 2 #################
BLUEs_GroundPH3 <- read.csv("rawDataY16_Ground_PH.csv")
str(BLUEs_GroundPH3)

chart.Correlation(as.matrix(BLUEs_GroundPH3[2:4]), histogram=TRUE, pch=19)

######################

#loading pachkges now:
library(Hmisc)
library(car)
library(ggplot2)
library(broom)
library(dplyr)
library(gridExtra)
library(ggpubr)
library(ggpmisc)
library(extrafont)


##Reading all the data and select the specific column of the data (agreetment with curretelly mensuration)
setwd
alldata<-read.csv("bluesData_allGS_Ground_PH.csv",header=T)
str(alldata)
#########Analise conjunta######
str(alldata)
alldata$GS<- factor(
  alldata$GS, 
  levels = c("E40","B", "A7", "M")
)

# 
# ggplot(alldata, aes(y = PH_Ground, x = GS, fill= GS)) +
#   geom_boxplot() +  theme_bw() +  
#   facet_grid(~Trial, scales = "free") + 
#   scale_y_continuous(breaks=seq(20, 120, by=5)) +
#   labs(x="Crop season", y="Plant height (cm)") +
#   theme_update(legend.position="top" , legend.title=element_blank(), panel.grid.major.x=element_blank())
# 


# Change point shapes and colors
# plot everything on one page
boxplot = ggplot(alldata, mapping = aes_string(y = "PH_Ground", x = "GS")) 
theme = theme_set(theme_minimal())  
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank()) 

######

theme = theme_set(theme_minimal())
theme = theme_update(legend.position="right", panel.grid.major.x=element_blank())
#Data
boxplot = ggplot(alldata, mapping = aes_string(y = "PH_Ground", x = "GS")) 

#Stylized Boxplot
boxplot = boxplot + geom_boxplot(aes_string(colour="GS", fill="GS")) +
  stat_summary(geom = "crossbar", width=0.65, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })
 
#No X Axis
theme = theme_update(axis.text.x=element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank())

#theme = theme_update(axis.text.y=element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(), axis.title.y=element_blank())
#No Y Axis Label + Grey Axis Numbers
#theme = theme_update(axis.line.y = element_blank(), axis.title.y=element_blank(), axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"))

boxplot + facet_grid(~Trial, scales = "free") +
  scale_y_continuous(breaks=seq(20, 125, by=10)) +
  scale_color_manual(values = c( "#00AFBB","#23b816", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c( "#00AFBB","#23b816", "#E7B800", "#FC4E07")) +
  labs(x="Winter cycle", y="PH ground-truth (cm)") +
  theme(text=element_text(family="Times New Roman", face="bold", size=12)) 


#E40 #00AFBB
#B #23b816
#A7 #E7B800
#R #FC4E07

########################################
###################
##########
####
#

library(corrplot)
library(RColorBrewer)
library(corrplot)
library(PerformanceAnalytics)

BLUEs_GroundPH
BLUEs_GroundPH1
BLUEs_GroundPH2

corr1 <- BLUEs_GroundPH[,2:4]
str(corr1)

# matrix of the p-value of the correlation
res<- cor.mtest(corr1)
correlation_matrix <- cor(corr1)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation_matrix, method="color", col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,  cl.lim = c(0, 1),
         family="serif",number.font=6,
         cl.align = "l"
)


BLUEs_GroundPH2
BLUEs_GroundPH3

corr2 <- BLUEs_GroundPH2[,2:5]
str(corr2)

# matrix of the p-value of the correlation
res<- cor.mtest(corr2)
correlation_matrix <- cor(corr2)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation_matrix, method="color", col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,  cl.lim = c(0, 1),
         family="serif",number.font=6,
         cl.align = "l"
)

BLUEs_GroundPH3

corr3 <- BLUEs_GroundPH3[,2:5]
str(corr3)

# matrix of the p-value of the correlation
res<- cor.mtest(corr3)
correlation_matrix <- cor(corr3)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(correlation_matrix, method="color", col=col(200),  
         type="lower", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = res$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,  cl.lim = c(0, 1),
         family="serif",number.font=6,
         cl.align = "l"
)

