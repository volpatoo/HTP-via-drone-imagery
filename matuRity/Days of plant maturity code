# The code below was written by Adam Herman and is a modification of code originally written by Leo. Volpato

####################
##  Housekeeping  ##
####################

rm(list = ls())

dir <- getwd()
setwd(dir)

# Check whether we have what we need
#If the R crashed, please return to control file and check all the fields
if(!file.exists("functions_PM.R")){
	write("Cannot find necessary functions file (functions_PM.R)", stderr())
	quit(status = 0, save = "no")
}

if(!file.exists("control.txt")){
	write("Cannot find necessary arguments file (control.txt)", stderr())
	quit(status = 0, save = "no")
}

## Set working directory (WD)
setwd(dir)

## Import the functions file
source("functions_PM.R")


####################
## General inputs ##
####################

## Read in control file

ctl <- read.delim('control.txt', comment.char = '#', sep = ':', header = F, col.names = c('arg', 'val'), as.is = T)

row.names(ctl) <- ctl$arg

ctl[,1] <- NULL


## Set the variables

use_procs = ctl['use.all',]

folder_shp <- ctl['DSN',]

layer_prefix_shp <- ctl['Layer',]

plot_name <- ctl['Plots_name',]

dir_ortho <- ctl['Dir_name',]

rgb_idx <- ctl['RGBindex',]

vi_method <- ctl['VI_Method',]

flights <- as.numeric(unlist(strsplit(ctl['Dates',], ', ')))

threshold <- as.numeric(ctl['Threshold',])

DPM_Model <- ctl['DPM_Model',]

## Set up multiprocess environment
procs <- availableCores()

if(use_procs == F){
	procs <- procs - 2
}

plan('multiprocess', workers = procs)

## Read the plots Shapefile
indPlots <- readOGR(dsn = folder_shp, layer = layer_prefix_shp)

## Polygon plot ID list from the shapefiles
names(indPlots)

## Data frame with plot names
indPlots_list <- as.data.frame(indPlots[,plot_name])

###Get all the files to process from the WD
imgFiles <-list.files(path = dir_ortho, pattern="*.tif$",full.names = T) #get the Orthosaics. Files that their name ends in group1.tif (Change all file names to otimization)


if(length(imgFiles) == 0){
	write("Cannot find necessary images (.tif files)", stderr())
	quit(status = 0, save = "no")
}

#################################
########## BLOCK 1 ##############
#################################


#VIs and metrod to pixels value extractions according to Volpato et al., 2020

ptm <- proc.time()
future.apply::future_lapply(imgFiles, function(x){
setwd(dir_ortho)
calculateIndex(x, output_suffix = paste0("_", rgb_idx), RGBindex = rgb_idx)
})
proc.time() - ptm

quantiles <- c('1q','5q', '25q','50q', '75q', '90q','95q','99q')

# This determines the lower and upper thresholds for the quantile value
if ( vi_method %in% quantiles ) {
	int.method <- as.numeric(gsub('q', '', vi_method))
	threshold_inf <- (1 - int.method / 100) / 2
	threshold_sup <- 1 - threshold_inf
} else {
message("Qualitile not defined: mean or median must be used to pixel value extration")}

# Define a vector on integers corresponding to plot indices.
plots <- 1:length(indPlots_list[,plot_name])

imgFiles_ind <-list.files(path = dir_ortho, pattern = rgb_idx)

ptm <- proc.time() 
for(i in 1:length(imgFiles_ind)){ 
  
  yz <- future.apply::future_sapply(plots, function(x){
    message("procesing ",x)
    
    setwd(dir_ortho)
		plot_crop_idx <- crop(raster(imgFiles_ind[i]),extent(indPlots[x,]))
		
		if ( vi_method == "median") {
		plot_crop_stat <- cellStats(plot_crop_idx, median) # get median of values for each plot     
		}
		
		if ( vi_method == "mean") {
		  plot_crop_stat <- cellStats(plot_crop_idx, mean) # get mean of values for each plot     
		}
		
		else {
			if (vi_method %in% quantiles){
				val_sup <- quantile(plot_crop_idx,threshold_sup) 
				val_inf <- quantile(plot_crop_idx,threshold_inf)
				plot_crop_idx[plot_crop_idx < val_inf | plot_crop_idx > val_sup] <- NA
			}
		plot_crop_stat <- cellStats(plot_crop_idx, mean)
		}
		
		plot_crop_stat
		
    })
	
	indPlots_list[,paste0(vi_method,"_",imgFiles_ind[i])] <- yz
	
		message("Done - ", imgFiles_ind[i])

}
 
proc.time() - ptm


setwd(dir)
#Save VIs and pixels values extracted for each plot in the original folder
write.csv(indPlots_list, paste0("Plot_value_",rgb_idx,'_',vi_method,".csv"), quote = F, row.names = F)

#################################
########## BLOCK 2 ##############
#################################

MaturityData <- read.csv(paste0("Plot_value_",rgb_idx,'_',vi_method,".csv"), header = T)

MaturityData$RGBindex <- rgb_idx
colnames(MaturityData)[1] <-"Plots"
MaturityData$IDplots <- paste0(MaturityData$RGBindex, "_",MaturityData$Plots )
MaturityData <- subset( MaturityData, select = c(-RGBindex,-Plots))
MaturityData<-MaturityData[ , order(names(MaturityData))]
MaturityData_t<- data.frame(t(MaturityData[,-1]))
names(MaturityData_t) <- MaturityData[, 1]
MaturityData_t[] <- lapply(MaturityData_t, function(x) type.convert(as.character(x)))

# Accordgin to the lenght of the flights (data points collected or each mission of flights performed)
flight_ini<-flights[1]
flight_end<- length(flights)
flight_end<-flights[flight_end]
days<- seq(flight_ini,flight_end,by=1)

##########################
#######  LOESS ###########
##########################

if ( DPM_Model == "LOESS") {

IDplots <- colnames(MaturityData_t)
magic_for(print, silent = TRUE)
ptm <- proc.time()
for (k in IDplots[1:ncol(MaturityData_t)]){
  nge_loess_loop<-loess(MaturityData_t[,k] ~ flights)
  fitted.nge_loop<-predict(nge_loess_loop, days)
  list_date_pred<-approx(fitted.nge_loop, days, xout = threshold) #xout = threshold selected
  print(list_date_pred$y)
}  
proc.time() - ptm


Mat_Est<- magic_result_as_dataframe()
colnames(Mat_Est)[1:2] <- c("Plots", paste0("DPM_",DPM_Model,'_',rgb_idx,'_',vi_method))

if ( NA %in% Mat_Est[,2]) {
  message("Consider changing the threshold")
  
}

write.csv(Mat_Est, paste0("DPM_",DPM_Model,'_',rgb_idx,'_',vi_method,".csv"), quote = F, row.names = F)


##########################
#######  SEG. REG. #######
##########################
}  else if( DPM_Model == "SEG") {
  
plots <- 1:length(indPlots_list[,plot_name])

defaultW <- getOption("warn") 
options(warn = -1) 

  ptm <- proc.time()
  wx <- future.apply::future_lapply(plots, function(x){
     # print(x)
  	data_col <- data.frame(MaturityData_t[,x])
  	data_col$Dates <- flights
  	names(data_col)[1] <- "Index"
  	mod<-lm(Index ~ flights, data = data_col) #glm also can be used to fit generalized linear models, but check if the script will work before)
  	attempts = 0
  	if.false <- F
  	while(if.false == F){
  	attempts <- attempts + 1
  		tryCatch({
              if(nrow(MaturityData_t) > 7 && attempts < 100){
  				#Volpato et al., 2020 paper recomendation is at least 8 flights to use 2 breaks points -- (3 optimum phases in the regression model = vegetative, senescense and maturity)
  				seg_loop<-segmented(mod, seg.Z = ~ flights, npsi = 2, control = seg.control(n.boot = 50, random=T, tol=0.01))
  			} else {
  				seg_loop<-segmented(mod, seg.Z = ~ flights, control = seg.control(n.boot = 50, random=T, tol=0.01))
  			}
  			slps <- slope(seg_loop)$flights
  			ncpt <- intercept(seg_loop)$flights
  			
  			if ( rgb_idx == "GLI" || rgb_idx == "TGI" ) {
  				slope <- min(slps[,1])
  			} else {
  				#i.e., RGBindex == "HI"
  				slope <- max(slps[,1])
  			}
  			slope_interc <- which(slps[,1] == slope)
  			B1_interc <- ncpt[slope_interc,1]
  		if.false <- T	
  		}, error = function(e){
  		}, finally = {})
  	}
  	DPM <- (threshold - B1_interc) / slope
  	list(w = names(MaturityData_t)[x], x = DPM)
    }, future.stdout = F)


options(warn = defaultW)

lm_all <- as.data.frame(do.call(rbind, wx))
lm_all <- data.frame(lapply(lm_all, as.character), stringsAsFactors=FALSE)
colnames(lm_all)<- c("Plots", paste0("DPM_",DPM_Model,'_',rgb_idx,'_',vi_method))

 if ( NA %in% lm_all[,2]) {
  message("Consider changing the threshold")}

write.csv(lm_all, paste0("DPM_",DPM_Model,'_',rgb_idx,'_',vi_method,".csv"), quote = F, row.names = F)

proc.time() - ptm
}
