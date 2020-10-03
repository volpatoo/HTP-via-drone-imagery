##Supplementary File
##Supplementary File 1. R code to replicate the plant height method used on the current pipeline

####Plant height estimation pipeline in R - CIMMYT 2020####
####Functions elaborated to automatic workflow to generate the crop surface model - August - 2020####

###################Libraries
require(raster)
###################Functions
### FUNCTION to calculate the difference between the DTM and DSM to get the CSM AKA plant height raster##########
## INPUTS are the filenames of the rasters (DSM, DTM) and the suffix to add to the DSM filename as output file
## Returns the generated CSM filename
calculateHeight <- function(dsmF,dtmF,output_suffix){
  #Will take note of the time it takes to complete the function
  start.time <- Sys.time() #Check the time when the script starts
  
  #Calculate difference
  dsm <- raster(dsmF) #Get DSM raster from the filename
  dtm <- raster(dtmF) #Get DTM raster from the filename
  message("To calculate the height raster, first match exactly the 2 rasters spatially (resample)...")
  dtm_fit <-resample(dtm, dsm, method="ngb") # Resample the DTM to match the DSM pixel by pixel at the exact location exactly
  message("calculating csm...")
  csm <- overlay(dsm, dtm_fit, fun=function(x,y){return(x-y)}) #Perform the Subtraction of DSM - DTM = CSM
  message("assume no negative values. Assign NA")
  csm[csm <=0] = NA # Remove the negative values of the raster, replace by NA. This can be provoked by noise in the low areas.
  
  #Save raster
  csm_filename <- appendSuffixTofilename(dsmF,output_suffix) #append output suffix to outputname
  message("Saving: ",csm_filename)
  writeRaster(csm, file=csm_filename,datatype='FLT4S',format="GTiff",overwrite=TRUE) #Save CSM raster to disk
  
  end.time <- Sys.time() #Take note of the time when finished
  time.taken <- end.time - start.time #Count the time spent
  print (time.taken) #print time
  
  return(csm_filename)
}


################### Function to append a sufix to the filename ###################
appendSuffixTofilename <- function(fullFileName, sx){
  fileN <- basename(fullFileName)
  ext <- tools::file_ext(fullFileName)
  basename <- tools::file_path_sans_ext(fileN)
  dir <- getwd()
  return(paste0(dir,"\\",basename,sx,".",ext))
}
