# PisoAI
[PisoAI](https://isotope.bot.unibas.ch/PisoAI/ "Piso.AI website") is a tool for predicting monthly time series of oxygen and hydrogen isotope values of precipitation that uses a machine learning model trained on geographic and climate data. Point predictions based on the model can be accessed here using the web interface [https://isotope.bot.unibas.ch/PisoAI/](https://isotope.bot.unibas.ch/PisoAI/ "Piso.AI website")

This repository contains all of the R code detailing the processing of input data and training of model used for the creation of the operational [PisoAI](https://isotope.bot.unibas.ch/PisoAI/ "Piso.AI website") model/dataset, to be released as:

```
Nelson DB, Basler D, Kahmen A (2020) Accurate monthly precipitation isotope spatial timeseries predictions with machine learning. Ready for submission.
```

## Overview of R files
* `0-file_paths.R` Edit this file to adjust paths fro data and other external resources
* `0-functions.R`  This script defines various functions that are called to extract data from the input predictor variable datasets
* `1-data_to_rts.R` This file transforms all data from the original format into the used raster time series (rts) format; It also crops the spatial extent to Europe
* `2-read in spatial data.R` This file reads in the cropped rts files. This method of extracting point data from rasters is slower than the method used in the Piso.AI application, but it allows for bilinear smoothing of the extracted data
* `3-read in points and extract spatial data.R`
* `4-d2H_initial_train.R`
* `5-d18O_initial_train.R`
* `6-d2H_simplified_train.R`
* `7-d18O_simplified_train.R`
* `8-transformFF.R` transform required input predictors to ff pobjects for use with Piso.AI application

Keep in mind that some of the scripts will take a significant amount of time to finish.

## Prerequisites
 The following data and tools have to be present on your system to use the code provided here.

#### Data

The following datasets have to be downloaded from data providers. The required files are not linked directly, to adhere to the different providers' data sharing policy. Some providers request to register before downloading data.

* [GNIP dataset](https://www.iaea.org/services/networks/gnip)
* [CRU dataset](http://www.cru.uea.ac.uk/data)
* [E-OBS dataset](https://www.ecad.eu)
* [NCEP Reanalysis data](https://www.esrl.noaa.gov/psd/)
* [CPC teleconnections dataset](https://www.cpc.ncep.noaa.gov/data/teledoc/telecontents.shtml)
* [USGS GTOPO30 DEM](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-global-30-arc-second-elevation-gtopo30?qt-science_center_objects=0#qt-science_center_objects)


#### Tools
* [CDO Tools](https://code.mpimet.mpg.de/projects/cdo/) CDO (Climate Data Operators) is a collection of command-line Operators to manipulate and analyze climate and forecast model Data.

#### R libraries

The required r libraries are listed below. The snippet can be used to install missing libraries. 
```
list.of.packages <- c("abind","caret","data.table","dplyr","ff","ggplot2","lubridate","ncdf4","raster","reshape2","rsample","rts","xgboost")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

## Acknowledgements

This research was supported through the ERC consolidator grant HYDROCARB (ERC-2016-COG- 724750).
