
#define external file paths at first instance 

#Script: 1-data_to_rts.R

#This directory contains the subdirectories "CRU" "EOBS" "NCEP" and "TELE" 
#Each of these subdirectores contains original data files as downloaded from source
pred_src_data_path<-"INSERT FILE PATH HERE" 

#This directory is the location where cropped data will be placed after transformation to raster time series (rts) objects
pred_data_out_path<-"INSERT FILE PATH HERE"



#Script: 3-read in points and extract spatial data.R

#Destination directory for writing exported csv containg all observations for complete training and validation data sets
output.directory.path<-"INSERT FILE PATH HERE"
#filename for csv
output.filename.path<- "INSERT FILE PATH HERE"
#directory and filename for csv containing all original observations from GNIP data after imputing missing values for d18O or d2H using Global Meteoric Water Line
site.data.path<-"INSERT FILE PATH HERE"

