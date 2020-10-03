####Plant height estimation pipeline in R - CIMMYT 2020####
####R script to execute the plant height function####
###################
####Set working directory
setwd("D:\\workingdirectory")

####Import the functions file
source("functions_PH.R")

####Import libraries needed
library(rgdal)
library(car)
library(Hmisc)
library(fBasics)

####General inputs  

####Read the individual plots shapefiles polygons
indPlots <- readOGR(dsn = "D:\\shp_folder", layer = "buffered_shapefilename")
####Get the polygon plot ID "Name" list from the shapefiles. The column name might change depending on the shapefile.
indPlots_list <- as.data.frame(indPlots$Name)
####Read filter list, to select specific plots
filterList <- read.csv("FilterList.csv")
####Set DTM filename (Digital terrain model generated from Pix4D before the vegetation grown)
dtm <- "D:\\img\\DTM.tif"

#####################Calculate height from DSM and DTM ---- Buffured shapefile
####Get the DSM filenames from all dates of one platform from the WD to process.
####Digital surface model generated from Pix4D throughout of the growth stage of vegetation 
####The name structure used is like this: cameramodel_site_date_dsm.tif
imgFiles <-list.files(pattern="dsm\\.tif$") #get the DSMs. Files that their name ends in dsm.tif
####Calculate height for each DSM
for(i in imgFiles){
  ####first construct the CSM from the DSM and DTM
  ####print DSM name
  message("processing DSM:",i)
  ####execute the substraction of dsm and dtm
  csm_filename <- calculateHeight(i,dtm, output_suffix= "_csm") # the output CSM file will be saved to the inputs location + suffix
  
  ####Iterate through all the plots to EXTRACT the height, using the ID list of the plots
  for (p in 1:length(indPlots_list$`indPlots$Name`)) {
    tryCatch({
      message("procesing ",p)
      ####crop the CSM to the extent of every plot
      plot_crop <- crop(raster(csm_filename),extent(indPlots[p,])) #raster of the "p" individual plot
      
      ####Get the 75% quantile to use as threshold (the quantile can be handled according to user)
      p75 <- quantile(plot_crop,.75)
      ####Get the height using 25% of top observations
      plot_crop_p75 <- plot_crop #copy the individual plot raster to work on it
      plot_crop_p75 [plot_crop_p75 < p75] <- NA #Remove values lower than the 75% quantile threshold. set them to NA.
      plot_crop_p75_mean <- cellStats(plot_crop_p75,mean) #get the mean of the 25% top quantile
      ####Save height for the "p" plot
      indPlots_list[p,paste0("height25top",i)] <- plot_crop_p75_mean 
    
    })
  }
}

####Save list of plant height to disk asn CSV text file
write.csv(indPlots_list,"Extracted_Plant_height.csv",row.names = FALSE)
