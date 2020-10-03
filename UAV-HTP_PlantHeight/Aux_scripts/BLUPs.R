setwd()
library(asreml)
library(nadiv)

eBee1Y_data<-read.csv("lmFLY_Y1_A7_eBee.csv") ## reads in the raw plant height data
str(eBee1Y_data)

blups.all <-NULL ## initializes a variable to store all of the BLUPss 

blups<-data.frame(unique(eBee1Y_data$GID)) ## initializes a dataframe to store the BLUPss
colnames(blups)<-"GID"

##for the loop
for(k in grep("PH_", colnames(eBee1Y_data))){ ## iterates through each of the plant height 
  message("Started processing")
  #Will take note of the time it takes to complete the function
  start.time <- Sys.time() #Check when the script starts
  
  eBee1Y<-eBee1Y_data[,c(which(colnames(eBee1Y_data)%in%c("GID", "REP", "SUB")), k)] ## creates a subsetted dataframe
  colnames(eBee1Y)[grep("PH_", colnames(eBee1Y))]<-"pheno"
  
  eBee1Y$GID<-factor(eBee1Y$GID) ## these need to be set as factors for asreml
  eBee1Y$REP<-factor(eBee1Y$REP)
  eBee1Y$SUB<-factor(eBee1Y$SUB)

  mod<-asreml(fixed=pheno ~  REP + 1, 
              random = ~ GID + REP/SUB, 
              data=eBee1Y,  
              maxiter=1000,
              singular.ok=TRUE) 
  
  h2a_mod<- pin(mod, h2a ~ V2/(V2 + (V4/2)))
  summary(mod)
  summary(mod)$varcomp

  ## fits GID as a random effect plus pedigree, REP fixed and INT and BLOC random effects
 
  ph.blup<- (predict(mod, classify="GID", sed=T))$predictions$pvals[,1:2]## creates a dataframe linking the BLUPss with their gids
  ph.blup$h2a<-h2a_mod$Estimate[1] ## adds the Test information
  ph.blup$SE<-h2a_mod$SE[1] ## adds the Test information
  #colnames(ph.blup)<-c("GID", paste(colnames(eBee1Y_data)[k])) ## sets the column name as the quantile PH extracted
  colnames(ph.blup)<-c("GID", paste(colnames(eBee1Y_data)[k]), paste(colnames(eBee1Y_data)[k], prefix0 = "_h2a"), 
                        paste(colnames(eBee1Y_data)[k], prefix0 = "_SE")) ## sets the column name with indice and extractions
  
  
  blups<-merge(blups, ph.blup, by="GID") # merges the BLUPss to the BLUPss dataframe
  summary(mod)$varcomp
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time #Count the time past
  print (time.taken) #print time
  
}

blups_all<-rbind(blups.all, blups) ## row-binds the BLUEs onto the dataframe containing all BLUEs

write.csv(blups_all, "BLUPs_A7_eBee_1Y.csv", row.names=F) ## writes the BLUEs into a CSV

anova.asreml(mod)

