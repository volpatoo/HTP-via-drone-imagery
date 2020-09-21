####Functions by Leonardo Volpato - U of MN - March 23, 2020###

pkgs <- c('ggplot2', 'tidyverse', 'devtools', 'raster', 'rgdal','magicfor', 'nadiv', 'segmented', 'future', 'future.apply')

new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]

if(length(new.packages)){
	install.packages(new.packages, repos='https://cloud.r-project.org/', dependencies = T)
	}


## ----load-libraries------------------------------------------------------
library(raster)# work with raster data
library(rgdal)# export GeoTIFFs and other core GIS functions
library(ggplot2)
library(magicfor)
library(nadiv)
library(segmented)
library(tidyverse)
library(future)
library(future.apply)

## FUNCTION to calculate the Vegetation indices #####################################
# Using stack function to read in all bands
#Indice = RGBindex (defined by the user)

#GLI function 
f_GLI <- function(r,g,b) {
  (2*g-r-b)/(2*g+r+b)
}

#HI function 
f_HI <- function(r,g,b) {
  (2*r-g-b)/(g-b)
}

#TGI function 
f_TGI <- function(r,g,b) {
  (-0.5*(190*(r - g)-120*(r - b)))
}

calculateIndex <- function(img,output_suffix, RGBindex){

start.time <- Sys.time() #Check when the script starts

RGB_stack <- stack(img)

rgb_r=raster(RGB_stack, layer=1)
rgb_g=raster(RGB_stack, layer=2)
rgb_b=raster(RGB_stack, layer=3)

##- calculate the indices
if ( RGBindex == "GLI" ) {
  
  rgbI<- f_GLI(rgb_r,rgb_g,rgb_b)
    
} else if ( RGBindex == "HI" ) {
  rgbI<- f_HI(rgb_r,rgb_g,rgb_b)
  
}
  
else if ( RGBindex == "TGI" ) { 
  rgbI<- f_TGI(rgb_r,rgb_g,rgb_b)
} 

##- visualize the indices
#raster::plot(rgbI)

#Save raster
rgbI_filename <- appendSuffixTofilename(img,output_suffix) #append output suffix to outputname
message("Saving: ",rgbI_filename)
writeRaster(rgbI, file=rgbI_filename,datatype='FLT4S',format="GTiff",overwrite=TRUE)

end.time <- Sys.time()
time.taken <- end.time - start.time #Count the time past
print (time.taken) #print time

}

## Function to append a sufix to the filename ## Thanks to Lorena Gonzales Perez (CIMMYT - https://github.com/Lorenagzp)
appendSuffixTofilename <- function(fullFileName, sx){
  fileN <- basename(fullFileName)
  ext <- tools::file_ext(fullFileName)
  basename <- tools::file_path_sans_ext(fileN)
  dir <- getwd()
  return(paste0(dir,"/",basename,sx,".",ext))
}


##################
#########
####
