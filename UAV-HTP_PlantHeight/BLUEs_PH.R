
setwd()
library(asreml)

eBee1Y_data<-read.csv("lmFLY_Y1_A7_eBee.csv") ## reads in the raw plant height data
str(eBee1Y_data)

blues.all <-NULL ## initializes a variable to store all of the BLUEs 

     blues<-data.frame(unique(eBee1Y_data$GID)) ## initializes a dataframe to store the BLUEs
     colnames(blues)<-"GID"
     
    for(k in grep("PH_", colnames(eBee1Y_data))){ ## iterates through each of the plant height for the specific GS
      message("Started processing")
      #Will take note of the time it takes to complete the function
      start.time <- Sys.time() #Check when the script starts

      eBee1Y<-eBee1Y_data[,c(which(colnames(eBee1Y_data)%in%c("GID", "REP", "SUB")), k)] ## creates a subsetted dataframe
      colnames(eBee1Y)[grep("PH_", colnames(eBee1Y))]<-"pheno"

      eBee1Y$GID<-factor(eBee1Y$GID) ## these need to be set as factors for asreml
      eBee1Y$REP<-factor(eBee1Y$REP)
      eBee1Y$SUB<-factor(eBee1Y$SUB)
    
      
      mod<-asreml(fixed=pheno~GID, random=~ REP+REP/SUB, data=eBee1Y, workspace=32e+6, pworkspace=8e+6, maxiter=1000) 
      ## fits gid as a fixed effect and rep, and block as random
      mod$fitted.values
      mod$fix
      
      ph.blue<-mod$coefficients$fixed[length(mod$coefficients$fixed)]+mod$coefficients$fixed[-length(mod$coefficients$fixed)]  ## extracts the BLUEs and adds the intercept
      ph.blue<-data.frame(gsub("GID_", "", names(ph.blue)), ph.blue) ## creates a dataframe linking the BLUEs with their gids
      colnames(ph.blue)<-c("GID", paste(colnames(eBee1Y_data)[k])) ## sets the column name as the quantile PH extracted
      
 
      blues<-merge(blues, ph.blue, by="GID") # merges the BLUEs to the BLUEs dataframe
      summary(mod)$varcomp
      
      end.time <- Sys.time()
      time.taken <- end.time - start.time #Count the time past
      print (time.taken) #print time
      
    }
     
  
     
     blues_all<-rbind(blues.all, blues) ## row-binds the BLUEs onto the dataframe containing all BLUEs
     
   
write.csv(blues_all, "RS_BLUEs_A7_eBee_1Y.csv", row.names=F) ## writes the BLUEs into a CSV



#################Testing the models significance#################
setwd()
library(asreml)
eBee1Y_data<-read.csv("Y1_A7_blues_eBee_FLY.csv") ## reads in the raw plant height data
str(eBee1Y_data)
eBee1Y_data$GID<-factor(eBee1Y_data$GID) ## these need to be set as factors for asreml
eBee1Y_data$REP<-factor(eBee1Y_data$REP)
eBee1Y_data$SUB<-factor(eBee1Y_data$SUB)
eBee1Y_data$FLY<-factor(eBee1Y_data$FLY)
names(eBee1Y_data)
mod1<-asreml(fixed=PH_25_DTA66_eBee_Yi_A7~GID, random=~ FLY + REP+REP/SUB, data=eBee1Y_data, workspace=32e+6, pworkspace=8e+6, maxiter=1000, REML = FALSE) 
summary(mod)

##REP/SUB = REP + REP/SUB = REP + REP:SUB (SUB within REP)
mod2<-asreml(fixed=PH_25_DTA66_eBee_Yi_A7~1, random=~ FLY + REP+REP/SUB, data=eBee1Y_data, workspace=32e+6, pworkspace=8e+6, maxiter=1000, REML = FALSE) 
summary(mod2)

anova.asreml(mod1)
anova.asreml(mod2)
#mod3<-asreml(fixed=pheno~GID, random=~ SUB, data=eBee1Y, workspace=32e+6, pworkspace=8e+6, maxiter=1000) 
#summary(mod3)

(l1 = mod1$logl)
(l2 = mod2$logl)
(l3 = mod3$logl)

(K1 = length(mod1$gammas))
(K2 = length(mod2$gammas))
(K3 = length(mod3$gammas))

LRT1 = -2* (summary(mod1))$loglik
LRT1

LRT2 = -2* (summary(mod2))$loglik
LRT2

LRT3 = -2* (summary(mod3))$loglik
LRT3

##AIC##

(AIC = -2*l1 + 2*K1)
(AIC = -2*l2 + 2*K2)
(AIC = -2*l3 + 2*K3)
