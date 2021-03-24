## Welcome to the help-page of ClimDatDownloadR


To get started please proceed [here](./README.md#a-warm-welcome). 


_This R-package was developed as a student project for the masters programm Geography at the Universität Hamburg, Germany._

## Update 09.03.2021
As of today a new alpha version is released. 
If you want to test the ClimDatDownloadR-package please feel free to install the package via

`install.packages("https://gitlab.rrz.uni-hamburg.de/helgejentsch/climdatdownloadr/-/archive/master/climdatdownloadr-master.tar.gz", repos = NULL, type = "source")`

and if you need the dependencies via 

`install.packages(c("gdalUtils", "httr", "ncdf4", "qpdf", "raster", "RCurl", "RefManageR", "rgdal", "stringr", "sf", "sp", "svMisc", "utils"), dependencies = TRUE)`


I would appreciate your feedback and possible bug reports. 
If you find anything, please send an email to [helge.marc.ole.jentsch@uni-hamburg.de](<mailto:helge.marc.ole.jentsch@uni-hamburg.de>)

Thank you very much for using ClimDatDownloadR! 

## A warm welcome
Hello and welcome to the ClimDatDownloadR R-package. 

With this package **cli**mate **dat**asets provided by [Chelsa](http://chelsa-climate.org/) and [WorldClim](https://www.worldclim.org/) can be automatically **download**ed, clipped, and converted with **R**.

To start, you'll have to install the package and it's dependencies first, if not already done. Then you can activate the package with the `library`-function. 

```{r setup}
# install.packages(c("gdalUtils", "httr", "ncdf4", "qpdf", "raster", "RCurl", "RefManageR", "rgdal", "stringr", "sf", "sp", "svMisc", "utils"), dependencies = TRUE)
# install.packages("https://gitlab.rrz.uni-hamburg.de/helgejentsch/climdatdownloadr/-/archive/master/climdatdownloadr-master.tar.gz", repos = NULL, type = "source")
library(ClimDatDownloadR)
```
Very well, now that you have the package installed and attached, let's start with the data sets of the climatologies of Chelsa and WorldClim. 

## Download Climatologies
In the help pages of [Chelsa.Clim.download()](../man/Chelsa.Clim.download.Rd) and [WorldClim.HistClim.download()](../man/WorldClim.HistClim.download.Rd) you can find further information about the handling of these functions. In fact running the functions all by itself bulk-downloads all the climatology data sets from the servers to your current working directory.

Let's start with a example of the Chelsa climatologies: 
```
Chelsa.Clim.download(
  # first you'll have to choose your working directory
  # don't worry about having a directory for every parameter you want to download
  # ClimDatDownloadR sorts this out for you
  save.location = "./", 
  # now you'll have to choose parameters. 
  # since there is the possibility to download more than one data set
  # the parameters must be a string-vector input. 
  # Single parameters, however, can be just put in as a string. 
  # the valid parameter inputs can be found in the help (linked s.o.)
  parameter = c("temp", "bio"),
  # Now, since you chose "temp" and "bio" as input parameters, 
  # you can specify the months and bioclim-variables to download. 
  # If you want all of them, just leave the default values.
  # It is crutial, however, that the inputs are integer number values.
  month.var = c(1), # Here January was chosen to be downloaded for demonstration purposes
  bio.var =  c(1), # Here the first bioclim-variable was chosen to be downloaded for demonstration purposes
  # For Chelsa a newer Version of their climatologies was published in 2019.
  # They still got their old version still hosted on their website. 
  # So you can download it as well, if you want to reproduce some research you base your studies on. 
  version.var = "1.2", # Here the newer version is chosen
  # Now you can choose whether you want the data set clipped
  clipping = TRUE, # Here TRUE was chosen to show a basic introduction to the function
  # Since "clipping" is enganged now you can specify the extent you want to have for your analysis
  # This is possible via the parameters "clip.shapefile", "clip.extent", and "buffer"
  clip.extent = c(-9,20,35,80), # Here the extent for Europe was used ... 
  buffer = 5, # ... with a 5 arc-degree buffer.
  # Now, since some might prefer older file formats there is a possibility to convert 
  # clipped files and raw data into ESRI-ASCII format
  convert.files.to.asc = FALSE, 
  # now you can stack the data ...
  stacking.data = FALSE, 
  # ... and choose if you want to combine the raw data in a .zip-file ...
  combine.raw.zip = FALSE,
  # and whether raw data should be deleted.
  delete.raw.data = FALSE,
  # Finally you are presented with the option to save a bibliography file at the save location. 
  save.bib.file = TRUE
)
```
___
With this showing the basic principle of these functions, here is a example of a WorldClim climatology download: 
```
WorldClim.HistClim.download(
  # As you can see, the structure of this function is very similar to the Chelsa-function
  save.location = "./",
  parameter = c("temp", "bio"),
  month.var = c(1),
  bio.var = c(1),
  # Here the resolution of the downloaded data set must be added
  # If no input is given all resolutions will be downloaded
  resolution = "10m", # here 10 arc-minutes are chosen
  # WorldClim also recently had an update to version 2.1
  version.var = "2.1", # Here the newer version is chosen
  clipping = TRUE,
  clip.extent = c(-9,20,35,80),
  buffer = 5,
  convert.files.to.asc = FALSE,
  stacking.data = FALSE,
  # here you can choose if you want to keep the downloaded zip-file
  keep.raw.zip = FALSE,
  delete.raw.data = FALSE,
  save.bib.file = TRUE
)
```
