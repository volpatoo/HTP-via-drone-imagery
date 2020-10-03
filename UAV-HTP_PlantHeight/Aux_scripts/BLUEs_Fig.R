###previosly these packeges conflicted withen others
#loading now:
library(ggplot2)
library(outliers)
library(ggrepel)
library(MASS)
library(ggpubr)
library(ggpmisc)
library(dplyr)
library(broom)
library(car)

##Reading all the data and select the specific column of the data (agreetment with curretelly mensuration)
setwd()
BLUEs_eBee1Y <- read.csv("agBLUEs_A7_eBee_1Y.csv")
str(BLUEs_eBee1Y)

modPH = lm(PH_A7 ~ PH_25_eBee_1Y_A7, BLUEs_eBee1Y)
summary(modPH)

##Outliers removed according to the  Bonferroni p-values test significance
BLUEs_eBee1Y_noOut<-BLUEs_eBee1Y
#indPlots_list_order_noOut[72,]<- NA

out <- outlierTest(modPH, cutoff = Inf, n.max = Inf)
table_pvalue <-out$p #Save the p values in a vector
filter_p <- table_pvalue[table_pvalue<0.05] # filter to have a TRUE/FALSE list of the p-values < 0.05
count_outliers<- sum(table_pvalue<0.05, na.rm = TRUE) #count outliers to be removed
BLUEs_eBee1Y_noOut[BLUEs_eBee1Y_noOut$GID %in% BLUEs_eBee1Y_noOut$GID[as.numeric(names(filter_p))],] <- NA #remove the plots that have p-values < 0.05
message(count_outliers, " Outliers will be removed")

modPH_noOut = lm(PH_A7 ~ PH_25_eBee_1Y_A7, BLUEs_eBee1Y_noOut)
summary(modPH_noOut)
write.csv(BLUEs_eBee1Y_noOut,"agBLUEs_A7_eBee_1Y_NoOut.csv", row.names = T)

modPH_25_results<- glance(modPH_noOut)[c(1,5,8,9,11)]
modPH_25_RMSE<- (sqrt(sum(modPH_noOut$residuals^2) / modPH_noOut$df))

#Salve results
modPH_25_results$RMSE <- modPH_25_RMSE
modPH_25_results$count_outliers<- count_outliers
write.csv(modPH_25_results,"cor_A7_eBee_1Y_results.csv", row.names = T)


#Regression Plot 

e = (sqrt(sum(modPH$residuals^2) / modPH$df))
#as.character(as.expression(e)

b <- ggplot(BLUEs_eBee1Y_noOut, aes(y = PH_A7, x = PH_25_eBee_1Y_A7))
b + geom_point(shape=24, color="#E7B800", fill ="#E7B800", size =4 ) +
  geom_smooth(method = "lm", color="#847673", fill = "lightgray") +  theme_bw() +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.y = 104, label.x.npc = "left",size = 5) + 
  stat_regline_equation(label.y = 102, label.x.npc = "left", size = 5 ) +
  scale_x_continuous(breaks=(seq(90, 120, 2))) +
  scale_y_continuous(breaks=(seq(90, 107, 2))) +
  labs(title = "eBee_A+7",
       x = "Aerial",
       y = "Ground") +
  theme(plot.title = element_text(hjust = 0.5)) 

#ggsave("noOUT_eBee_A+7_HiBAP_I_correl.png", ggplot, dpi=500)


#E40 #00AFBB #16 or 19
#B #23b816 #15
#A7 #E7B800 #17 or 24
#R #FC4E07 #18


